// set the dimensions and margins of the graph
var margin = {top: 20, right: 20, bottom: 30, left: 50},
    width = 1000 - margin.left - margin.right,
    height = 800 - margin.top - margin.bottom;

d3.csv("https://raw.githubusercontent.com/GWarrenn/this-and-that/drafting/english-football-trends/data/historical_table_data.csv", function(data){
    debug =  data

    var teamSelector = d3.select('#team')

    load = 0
    var selectValue = ''
    
    all_teams = [...new Set(debug.map(item => item.team))];	
    var selected_team = all_teams[0]

    teamSelector
        .selectAll("option")
        .data(all_teams.sort())
        .enter()
        .append("option")
        .attr("value", function (d) { return d; })
        .text(function (d) {
            return d[0].toUpperCase() + d.slice(1,d.length);
        })
    teamSelector.on('change',function() {
        team_filter = d3.select(this)
        selected_team = team_filter["_groups"][0][0].value
        plotResults(selected_team)
    })
    plotResults(selected_team)
})

function plotResults(team){

    d3.csv("https://raw.githubusercontent.com/GWarrenn/this-and-that/drafting/english-football-trends/data/historical_table_data.csv", function(data){

        // set the ranges
        chart = d3.select('#chart')
            .attr('width', width)
            .attr('height', height);
        
        chart.selectAll("*").remove()   

        var y = d3.scaleLinear()
            .range([margin.top, height - margin.bottom])
            .domain([1, 92]);

        var yAxis = d3.axisLeft()
            .ticks(10)
            .scale(y);

        var x = d3.scaleLinear()
            .range([margin.left , width - margin.right])
            .domain([1893,2019]);

        var xAxis = d3.axisTop()
            .ticks(10)
            .scale(x);

        // show weeks
        chart.append('g')
            .attr('class', 'axis x-axis')
            .attr('transform', 'translate(0,0)')
            .call(xAxis)
            .selectAll('text')
            .attr('dy', 2)

        // show position
        chart.append('g')
            .attr('class', 'axis y-axis')
            .attr('transform', 'translate(0, 0)')
            .call(yAxis)     

        chart.append('text')
            //.attr("x", 15)
            .attr("y", 15)
            .attr("transform", "rotate(-90)")
            .attr("fill", "#000")
            .text("Standing");    
            
        valueline = d3.line()
            .x(function(d) { return x(d.Season); })
            .y(function(d) { return y(d.england_position); });	
            
        league_bottom_line =  d3.line()
            .x(function(d) { return x(d.Season); })
            .y(function(d) { return y(d.adjusted_league_bottom); });	   

        debug.forEach(function(d) {
            d.adjusted_league_bottom = +d.adjusted_league_bottom;
            d.england_position = +d.england_position;
            d.Season = +d.Season;
        });

        nest = d3.nest()
            .key(function(d){
                return d.team;
            })
            .sortValues(function(a,b) { return a.Season > b.Season ? 1 : -1; })
            .entries(debug)
            
        tier_nest = d3.nest()
            .key(function(d){
                return d.tier;
            })
            .sortValues(function(a,b) { return a.Season > b.Season ? 1 : -1; })
            .entries(debug) 
            
        selected_team_nest = nest.filter(function(d) { return d.key == team })

        chart.selectAll(".england_positions")
            .data(nest)
            .enter()
            .append("path")
            .attr("class", "line")
            .style("opacity", 0.1)
            .attr("d", function(d){
                return valueline(d.values)  
            })
            .attr("stroke", "black")
            .attr("fill", "none");

        chart.selectAll(".selected_team_position")
            .data(selected_team_nest)
            .enter()
            .append("path")
            .attr("class", "line")
            .style("opacity", 1)
            .attr("d", function(d){
                return valueline(d.values)  
            })
            .attr("stroke", "red")
            .attr("stroke-width", "2")
            .attr("fill", "none");    

        chart.selectAll(".league_bottoms")
            .data(tier_nest)
            .enter()
            .append("path")
            .attr("class", "line")
            .style("opacity", 1)
            .attr("d", function(d){
                return league_bottom_line(d.values)
            })
            .attr("stroke", "black")
            .attr("fill", "none");                 
    })
}