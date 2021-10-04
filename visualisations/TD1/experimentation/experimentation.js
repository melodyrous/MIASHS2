var baseSize = 40;

var nbTrials = 20;
if(nbTrials%2==1) nbTrials-=1;
var curTrial = 0;

var trialTime = 10000;
var trialCurTime = 0;

var curRatio;
var timeOut;
var timeInterval;
var startTime;

var title;
var svg;
var time;
var res;
var radios;

var barsAccuracy = new Array ();
var cirsAccuracy = new Array ();

var barsRespTime = new Array ();
var cirsRespTime = new Array ();

function init(t, s, tm, r, ra, w, h,){

	title = t;
	svg = s;
	time = tm;
	res = r;
	radios = ra;

	svg.attr("width", w)
	   .attr("height", h);
	
}

/*
 * selRatio = ratio selected by user
 * 0 = timeout
 * null = first trial
 */
function nextTrail(selRatio){
	
	clearTimeout(timeOut);
	clearInterval(timeInterval);
	
	// Gestion des résultats
	if (selRatio!=null) { // Si ce n'est pas la première itération
		if (selRatio!=0) { // Si l'utilisateur a donné une réponse avant la fin du timeout
	 		if(curTrial>nbTrials/2) {// Si c'était 1D 
				barsAccuracy.push(Math.abs(curRatio-selRatio));
				barsRespTime.push((new Date().getTime() - startTime)/1000);
			}
			else { // Si c'était 2D 
				cirsAccuracy.push(Math.abs(curRatio-selRatio));
				cirsRespTime.push((new Date().getTime() - startTime)/1000);
			}
		}
	}
	
	// Lancement du nouvel essai
	if(curTrial<nbTrials) { // Si le nombre d'essais n'a pas été atteint
		
		// Pour afficher le reste du temps à 0 si l'utilisateur n'a pas répondu
		displayTime();
		
		// Popups
		if(selRatio==0) {
			alert("Timeout, vous avez mis trop de temps pour répondre !");
		}
		alert("Cliquez pour démarrer un nouvel essai");
		
		// Lancement de la nouvelle visualisation
		if(curTrial<nbTrials/2) drawCirs();
		else drawBars();
		
		// Lancement du décompte du temps
		timeOut = setTimeout("nextTrail(0)", trialTime);
		startTime = new Date().getTime();
		
		// Lancement de l'affichage du décompte du temps
		trialCurTime = trialTime;
		displayTime();
		timeInterval = setInterval("displayTime()", 1000);

		curTrial += 1;
	}
	else { // Si le nombre d'essais a été atteint (c'est la fin)
		trialCurTime = 0;
		displayTime();
		alert("Fini");
		drawRes();
	}
	
	// Réinitialise les boutons radio
   	for(var i=0;i<radios.length;i++) radios[i].checked = false;
}

function displayTime(){
	time.html("Temps restant : "+(trialCurTime/1000)+ " secondes.");
	trialCurTime -= 1000;
}

function drawCirs(){

	clear(svg);
	
	title.html("Taille 2D : aire");
	
	curRatio = getRandomRatio();	
	
	svg.append("circle")
		.attr("r", baseSize)
		.attr("fill","teal")
		.attr("cx",svg.attr("width")/2-80)
		.attr("cy",svg.attr("height")-baseSize);

	svg.append("circle")
		.attr("r", baseSize*Math.sqrt(curRatio))
		.attr("fill","teal")
		.attr("cx",svg.attr("width")/2+80)
		.attr("cy",svg.attr("height")-baseSize*Math.sqrt(curRatio));
}

function drawBars(){
	
	clear(svg);
	
	title.html("Taille 1D : longueur");
	
	curRatio = getRandomRatio();
	
	svg.append("rect")
		.attr("width", 10)
		.attr("height", baseSize)
		.attr("fill","teal")
		.attr("x",svg.attr("width")/2-30)
		.attr("y",svg.attr("height")-baseSize);
	
	svg.append("rect")
		.attr("width", 10)
		.attr("height", baseSize*curRatio)
		.attr("fill","teal")
		.attr("x",svg.attr("width")/2+30)
		.attr("y",svg.attr("height")-(baseSize*curRatio));
}

function drawRes(){

	clear(svg);
	
	title.html("Résultats");
	svg.remove();
	res.attr("style", "display: block");
	
	cirsNbSucc = nbSuccess(cirsAccuracy);
	cirsNbFail = cirsAccuracy.length - cirsNbSucc;
	cirsNbNull = (nbTrials/2)-cirsAccuracy.length;
	
	barsNbSucc = nbSuccess(barsAccuracy);
	barsNbFail = barsAccuracy.length - barsNbSucc;
	barsNbNull = (nbTrials/2)-barsAccuracy.length;
	
	var tmp = "";
	tmp += "<strong>Taille 2D (aire), "+ (nbTrials/2) + " essais dont :</strong>";
	tmp += "<ul>";
	tmp += "<li>"+cirsNbNull+" non répondu(s),</li>";
	tmp += "<li>"+cirsNbSucc+" succès,</li>";
	tmp += "<li>"+cirsNbFail+" échec(s),</li>";
	tmp += "<li>Erreur moy. : "+moyenne(cirsAccuracy)+", med. : "+median(cirsAccuracy)+",</li>";
	tmp += "<li>Temps moy. : "+moyenne(cirsRespTime)+", med. : "+median(cirsRespTime)+".</li>";
	tmp += "</ul>";
	tmp += "<strong>Taille 1D (longueur), "+ (nbTrials/2) + " essais dont :</strong>";
	tmp += "<ul>";
	tmp += "<li>"+barsNbNull+" non répondu(s),</li>";
	tmp += "<li>"+barsNbSucc+" succès,</li>";
	tmp += "<li>"+barsNbFail+" échec(s),</li>"; 
	tmp += "<li>Erreur moy. : "+moyenne(barsAccuracy)+", med. : "+median(barsAccuracy)+",</li>";
	tmp += "<li>Temps moy. : "+moyenne(barsRespTime)+", med. : "+median(barsRespTime)+".</li>";
	tmp += "</ul>";
	
	res.html(tmp);
}

function clear(){
	svg.selectAll("*").remove();
}

function getRandomRatio(){
	r = [1.5,2,2.5,3,3.5,4,4.5,5];
	return r[getRandomInt(r.length)];
}

function getRandomInt(max) {
  return Math.floor(Math.random() * max);
}

function nbSuccess(values) {
	var c = 0;
	for(i=0 ; i<values.length ; i++){
		if(values[i]==0) c++;
	}
	return c;
}

function moyenne(values) {
	var s = 0;
	for(i=0 ; i<values.length ; i++){
		s += values[i];
	}
	return (s/values.length);
}

function median(values) {
    values.sort( function(a,b) {return a - b;} );
    var half = Math.floor(values.length/2);
    if(values.length % 2)
        return values[half];
    else
        return (values[half-1] + values[half]) / 2.0;
}