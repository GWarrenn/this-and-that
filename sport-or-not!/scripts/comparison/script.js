
function random_sport(){

	d3.select("#sport-stats svg").remove();

	var sports = ["Chess","eSports..Videogames.","Ping.Pong..Table.Tennis.","Foosball","Skiing",
            "Snowboarding","Cycling","Bowling","Golf","Ultimate.Frisbee","Sailing",
            "Rowing..Crew.","Frisbee.Golf","Kickball","Scrabble","Cornhole","Pickleball",
            "NASCAR","Crossfit"]

	randomItem = sports[Math.floor(Math.random()*sports.length)];
	document.getElementById("sport").innerHTML = 'How do you rank <b>' + randomItem + "</b>?"
	document.getElementById("user-pick").innerHTML = ""


}

function msg(selected_value){

	var publicSpreadsheetUrl = 'https://docs.google.com/spreadsheets/d/1QVqwzJrUngsbUNSY8gSpiMgkeYCePmxpbFUuqawofuw/pubhtml';

	function renderSpreadsheetData() {
		Tabletop.init( { key: publicSpreadsheetUrl,
					 callback: draw,
					 simpleSheet: true } )
	}

	function draw(data, tabletop) {

		d3.select("#sport-stats svg").remove();

		results = tabletop.sheets("Form Responses 1")
		main_data = results.elements

		var denom = main_data.length	
		
		randomItem = randomItem.replace(".", " ")
		console.log(randomItem)	


		var_select = 'Are these things sports? [' + randomItem + ']'

		all = _.map(_.groupBy(main_data,var_select), function (item, key) {
		    var size = item.length,
		        obj={};
		    var positiveCount = _.countBy(item, function (i) {
		        return i.answer == "Sport - Feel Strongly" ? "Sport - Feel Strongly" : "count";
			});
		    console.log(positiveCount)
		    var percentagePossitive = (positiveCount.count / denom) * 100;

	    	obj['key'] = key
	    	obj['value'] = percentagePossitive || 0;
	    	return obj;
			});

		selected_value = selected_value.replace("Dont", "Don't")

		user_pick = _.filter(all, { 'key': selected_value});
		console.log(user_pick[0].value)
		document.getElementById("user-pick").innerHTML = "<b>" + Math.round(user_pick[0].value) + "% </b> of respondents also ranked " + randomItem + " as a " + user_pick[0].key + " sport"

		var order = ["Sport - Feel Strongly","Sport - Don't Feel Strongly", "Not a Sport - Don't Feel Strongly","Not a Sport - Feel Strongly","Never heard of/Don't know what this is"];
		all = _.sortBy(all, function(obj){ 
		    return _.indexOf(order, obj.key);
		});

		// set the dimensions and margins of the graph
		var margin = {top: 20, right: 20, bottom: 30, left: 40},
			width = 960 - margin.left - margin.right,
			height = 500 - margin.top - margin.bottom;

		// set the ranges
		var x = d3.scaleBand()
		      .range([0, width])
		      .padding(0.1);
		var y = d3.scaleLinear()
		      .range([height, 0]);

		var color = d3.scaleOrdinal()
			.domain(["Sport - Feel Strongly","Sport - Don't Feel Strongly", "Not a Sport - Don't Feel Strongly","Not a Sport - Feel Strongly","Never heard of/Don't know what this is"])
			.range(["#1a9641","#a6d96a","#fdae61","#d7191c","#D3D3D3"]);      
		      
		// append the svg object to the body of the page
		// append a 'group' element to 'svg'
		// moves the 'group' element to the top left margin
		var svg = d3.select("#sport-stats")
			.append("svg")
			.attr("width", width + margin.left + margin.right)
			.attr("height", height + margin.top + margin.bottom)
			.append("g")
			.attr("transform", 
			      "translate(" + margin.left + "," + margin.top + ")");

		// Scale the range of the data in the domains
		x.domain(all.map(function(d) { return d.key; }));
		y.domain([0, d3.max(all, function(d) { return d.value; })]);

		// append the rectangles for the bar chart
		svg.selectAll(".bar")
		  .data(all)
		.enter().append("rect")
		  .attr("class", "bar")
		  .attr("x", function(d) { return x(d.key); })
		  .attr("width", x.bandwidth())
		  .attr("y", function(d) { return y(d.value); })
		  .attr("height", function(d) { return height - y(d.value); })
		  .style("fill", function(d) { return color(d.key); })
		  .style("stroke", "black")

		svg.selectAll(".text")  		
		  .data(all)
		  .enter()
		  .append("text")
		  .attr("class","label")
		  .attr("x", (function(d) { return x(d.key) + x.bandwidth() / 2.5 ; }))
		  .attr("y", function(d) { return y(d.value) - 15 })
		  .attr("dy", ".75em")
		  .text(function(d) { return Math.round(d.value) + "%"; });


		// add the x Axis
		svg.append("g")
		  .attr("transform", "translate(0," + height + ")")
		  .call(d3.axisBottom(x));

		// add the y Axis
		svg.append("g")
		  .call(d3.axisLeft(y));

	}
	renderSpreadsheetData();
}

