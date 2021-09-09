var svgCloud;

var wCloud;
var hCloud;
				
var scaleXCloud = d3.scaleLinear();
var scaleYCloud = d3.scaleLinear();
var scaleCCloud = d3.scaleOrdinal(d3.schemeCategory10);
    			
var xAxisCloud = d3.axisBottom(scaleXCloud);
var yAxisCloud = d3.axisLeft(scaleYCloud);

var gxAxisCloud;
var gyAxisCloud;

var dataCloud;

var attXCloud;
var attYCloud;
var attCCloud;

var items;

function initCloud(svg, w, h, d, x, y, c){
	
	svgCloud = svg;
	wCloud = w;
	hCloud = h;
	dataCloud = d;
	attXCloud = x;
	attYCloud = y;
	attCCloud = c;

	/*
	 * SVG
	 */
	
	svgCloud.attr("width", wCloud)
			.attr("height", hCloud);

	/*
	 * Axe X
	 */

	scaleXCloud.domain([d3.min(dataCloud, function(d) { return d[attXCloud]; }), 
						d3.max(dataCloud, function(d) { return d[attXCloud]; })]);
    scaleXCloud.range([0, wCloud-50]);   
    
    gxAxisCloud = svgCloud.append("g")
		.call(xAxisCloud)
		.attr("transform","translate(25,"+(hCloud-25)+")");

	/*
	 * Axe Y
	 */

	scaleYCloud.domain([d3.min(dataCloud, function(d) { return d[attYCloud]; }), 
						d3.max(dataCloud, function(d) { return d[attYCloud]; })]);
    scaleYCloud.range([hCloud-50,0]);
    	
	gyAxisCloud = svgCloud.append("g")
		.call(yAxisCloud)
		.attr("transform","translate(25,25)");
		
	/*
	 * Cercles
	 */
	
	items = svgCloud.selectAll(".circles")
		.data(dataCloud)
		.enter().append("circle");
	
	items.attr("stroke", "#aaaaaa")
		.attr("fill", function(d) { return scaleCCloud(d[attCCloud]); } )
		.attr("r", 4)
		.attr("cx", function(d) { return 25+scaleXCloud(d[attXCloud]); } )
		.attr("cy", function(d) { return 25+scaleYCloud(d[attYCloud]); } );	
};

function updateCloudX(att){
	attXCloud=att;
	scaleXCloud.domain([d3.min(dataCloud, function(d) { return d[attXCloud]; }),
	 d3.max(dataCloud, function(d) { return d[attXCloud];
	})]);
	gxAxisCloud.call(xAxisCloud);
	items.transition().attr("cx", function (d){return 25+scaleXCloud(d[attXCloud]);});
	updateHistX(att);
};

function updateCloudY(att){
	attYCloud = att;
	scaleYCloud.domain([d3.min(dataCloud, function(d) {return d[attYCloud]; }),
	d3.max(dataCloud, function (d) {return d[attYCloud];
	})]);
	gyAxisCloud.call(yAxisCloud);
	items.transition().attr("cy", function (d) {return 25+scaleYCloud(d[attYCloud]); });
};