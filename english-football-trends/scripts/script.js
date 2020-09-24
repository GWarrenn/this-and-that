// set the dimensions and margins of the graph
var margin = {top: 20, right: 20, bottom: 30, left: 50},
    width = 1000 - margin.left - margin.right,
    height = 800 - margin.top - margin.bottom;

updateFilter('AFC Bournemouth')

function updateFilter(input_team){

    d3.csv("https://raw.githubusercontent.com/GWarrenn/this-and-that/master/english-football-trends/data/historical_table_data.csv", function(data){
        debug =  data
         
        var teamSelector = d3.select('#team')
        teamSelector.selectAll("*").remove()

        load = 0
        var selectValue = input_team
        
        all_teams = [...new Set(debug.map(item => item.team))];	
        var selected_team = input_team

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
            clubDescription(selected_team)
        })
        plotResults(selected_team)
        clubDescription(selected_team)
    })
}

//////////////////////////////////////////
//
// Club Description
//
//////////////////////////////////////////

function clubDescription(team){
    d3.csv("https://raw.githubusercontent.com/GWarrenn/this-and-that/master/english-football-trends/data/historical_table_data.csv", function(data){

        best_worst_text = ''
        no_data_text = ''

        data.forEach(function(d) {
            d.adjusted_league_bottom = +d.adjusted_league_bottom;
            d.england_position = +d.england_position;
            d.league_position = +d.league_position;
            d.moving_change = +d.moving_change;
            d.avg_position = +d.avg_position;
            d.Season = +d.Season;
        });            

        club = data.filter(function(d) { return d.team == team })

        // Best Decade of Growth    

        club_best = _.orderBy(club, ({ moving_change }) => moving_change || '', ['desc']);
        club_best = club_best.slice(0,1)

        decade_end = club_best[0].Season + 10
        best_decade = club_best[0].Season + "-" + decade_end
        best_decade_change = club_best[0].moving_change 

        // Worst Decade of Growth

        club_roughest = _.orderBy(club, ['moving_change'], ['asc']);
        club_roughest = club_roughest.slice(0,1)
 
        decade_end = club_roughest[0].Season + 10
        roughest_decade = club_roughest[0].Season + "-" + decade_end
        roughest_decade_change = Math.abs(club_roughest[0].moving_change)

        // Best Decade of Average Position

        club_peak_avg = _.orderBy(club, ['avg_position'], ['asc']);
        club_peak_avg = club_peak_avg.slice(0,1)

        decade_start = club_peak_avg[0].Season - 10
        peak_decade =  decade_start + "-" + club_peak_avg[0].Season
        peak_decade_avg = Math.round(club_peak_avg[0].avg_position) 

        // Worst Decade of Average Position

        club_gutter_avg = _.orderBy(club, ({ avg_position }) => avg_position || '', ['desc']); 
        club_gutter_avg = club_gutter_avg.slice(0,1)

        decade_start = club_gutter_avg[0].Season - 10
        gutter_decade =  decade_start + "-" + club_gutter_avg[0].Season
        gutter_decade_avg = Math.round(club_gutter_avg[0].avg_position)

        const nth = function(d) {
            if (d > 3 && d < 21) return 'th';
            switch (d % 10) {
              case 1:  return "st";
              case 2:  return "nd";
              case 3:  return "rd";
              default: return "th";
            }
          }

        if(!isNaN(roughest_decade_change) & !isNaN(best_decade_change)){
            best_worst_text = team + " had the best decade of sustained growth during <b>" + best_decade + "</b>, where they gained an average of <b>" + best_decade_change + 
            "</b> positions per season, on average. The roughest decade came in <b>" + roughest_decade + "</b>, where they dropped an average of <b>" + roughest_decade_change + 
            "</b> positions per season." + " The peak decade of average performance was in <b>" + peak_decade + "</b>, where they maintained an average season-end table position of <b>" +
            peak_decade_avg + nth(peak_decade_avg).toString() + "</b> place across the decade. However, the worst decade of average performance can in <b>" + gutter_decade + "</b>, where they maintained an average season-end table position of <b>" + 
            gutter_decade_avg + nth(gutter_decade_avg).toString() + "</b>. "
        }
        else{
            no_data_text = "<br><i>" + team + " does not have enough historical data to produce best/roughest decades.</i>"
        }

        highest_position = _.orderBy(club, ['england_position','Season'], ['asc','desc']);
        highest_position = highest_position.slice(0,1)

        highest_tier = highest_position[0].tier
        highest_league_season = highest_position[0].Season

        if(highest_tier == 1){
            if(highest_league_season > 1992) {
                highest_tier = "the Premier League"
            }
            else{
                highest_tier = "the Football League First Division"
            }    
        } 
        else if(highest_tier == 2){
            if(highest_league_season >= 2004) {
                highest_tier = "the Championship"
            }
            else if(highest_league_season > 1992 & highest_league_season < 2004){
                highest_tier = "the Football League First Division"
            }    
            else{
                highest_tier = "Football League Second Division"
            }
        } 
        else if(highest_tier == 3){
            if(highest_league_season >= 2004) {
                highest_tier = "League One"
            }
            else if(highest_league_season > 1992 & highest_league_season <= 2004){
                highest_tier = "the Football League Second Division"
            }    
            else{
                highest_tier = "Football League Third Division"
            }
        } 
        else if(highest_tier == 4){
            if(highest_league_season >= 2004) {
                highest_tier = "League Two"
            }
            else if(highest_league_season > 1992 & highest_league_season <= 2004){
                highest_tier = "the Football League Third Division"
            }    
            else{
                highest_tier = "Football League Fourth Division"
            }
        }                       
        highest_league_position = highest_position[0].league_position

        document.getElementById("club_description").innerHTML = best_worst_text + "The single best, most recent season (based on final table position alone) for " + 
            team + " was the <b>" + highest_league_season + "</b> season where they finished in <b>" + highest_league_position.toString() +
            nth(highest_league_position).toString() + "</b> place in <b>" + highest_tier + "</b>. " + no_data_text + "<br><br>"

    })
}

//////////////////////////////////////////
//
// Historical Trend Plot
//
//////////////////////////////////////////

function plotResults(team,input_season,input_fill){

    d3.csv("https://raw.githubusercontent.com/GWarrenn/this-and-that/master/english-football-trends/data/historical_table_data.csv", function(data){

        // set the ranges
        chart = d3.select('#chart')
            .attr('width', width)
            .attr('height', height);
        
        chart.selectAll("*").remove()   

        var parseTime = d3.timeParse("%Y")
        bisectDate = d3.bisector(function(d) { return d.Season; }).left;

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
            .tickFormat(function(d) { return d })
            .scale(x);

        // show weeks
        chart.append('g')
            .attr('class', 'axis x-axis')
            .attr('transform', 'translate(0,' + ((margin.top/2) + 8) + ')')
            .call(xAxis)
            .selectAll('text')
            .attr('dy', 0) 

        debug.forEach(function(d) {
            d.adjusted_league_bottom = +d.adjusted_league_bottom;
            d.england_position = +d.england_position;
            d.Season = +d.Season;
        });            

        valueline = d3.line()
            .x(function(d) { return x(d.Season); })
            .y(function(d) { return y(d.england_position); })
            .defined(function(d) { return d.england_position; })
            
        league_bottom_line =  d3.line()
            .x(function(d) { return x(d.Season); })
            .y(function(d) { return y(d.adjusted_league_bottom); }) 
            .defined(function(d) { return d.adjusted_league_bottom; })  

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

        //filled area for best/roughest decades

        if (input_season) {
            chart.append("line")
                .attr("x1", x(input_season)) 
                .attr("y1", 20)
                .attr("x2", x(input_season))
                .attr("y2", height - margin.top - margin.bottom + 20)
                .style("stroke-width", 1)
                .style("stroke", "black")
                .style("fill", "none");    
            chart.append("line")
                .attr("x1", x(input_season+10)) 
                .attr("y1", 20)
                .attr("x2", x(input_season+10))
                .attr("y2", height - margin.top - margin.bottom + 20)
                .style("stroke-width", 1)
                .style("stroke", "black")
                .style("fill", "none");    
             chart.append("rect")
                .attr("x", x(input_season))
                .attr("y", 20)
                .attr("width", x(input_season+10) - x(input_season))
                .attr("height", height - margin.top - margin.bottom)
                .style("fill", input_fill)
                .style("opacity",.2);   
        } 
    })
}

//////////////////////////////////////////
//
// Roughest/Best Decade Tables
//
//////////////////////////////////////////

d3.csv("https://raw.githubusercontent.com/GWarrenn/this-and-that/master/english-football-trends/data/historical_table_data.csv", function(data){

    table_data = data

    data.forEach(function(d) {
        d.moving_change = +d.moving_change;
        d.Season = +d.Season;
    });      

    ////////////////////////////
    //
    //best decade
    //
    ////////////////////////////

    d3.select("#best-decade-table tbody").remove();
    d3.select("#best-decade-table thead").remove();

    var table = d3.select('#best-decade-table')
        .append('table')

    var thead = table.append('thead')
    var	tbody = table.append('tbody');

    best_data = _.orderBy(data, ({ moving_change }) => moving_change || '', ['desc']);
    best_data = best_data.slice(0,40)

    display_cols = ['Season','Team','Average Positions Gained per Season']
    columns = ['Season','team','moving_change']

    best_color = d3.scaleLinear()
        .domain([8,5.5])
        .range(["#71e554","#ffffff"]);  
				
    //// append the header row
    thead.append('tr')
      .selectAll('th')
      .data(display_cols).enter()
      .append('th')
        .text(function (column) { return column; });

    // create a row for each object in the data
    var new_rows = tbody.selectAll('tr')
      .data(best_data)
      .enter()
      .append('tr')
      .classed("highlight", false)
      .on("click", function(d) { 
          element = document.getElementById('team');
          element.value = d.team;
          plotResults(d.team,d.Season,"green"); 
          clubDescription(d.team);
          new_rows.classed("highlight", false);
          d3.select(this).classed("highlight", true);})
      //.on("click", function(d) { updateFilter(d.team); })

      new_rows.exit().remove();

    cells = new_rows.selectAll('td')
    .data(function (row) {
        return columns.map(function (column) {
            return {column: column, value: row[column]};
        });
    })
    .enter()
    .append('td')
    .style("background-color", function(d){ if(d.column == "moving_change") return best_color(d.value);})
    .text(function (d) { return d.value; });

    cells.exit().remove();     

    ////////////////////////////
    //
    //roughest decade
    //
    ////////////////////////////

    d3.select("#roughest-decade-table tbody").remove();
    d3.select("#roughest-decade-table thead").remove();

    var table = d3.select('#roughest-decade-table')
        .append('table')

    var thead = table.append('thead')
    var	tbody = table.append('tbody');

    roughest_data = _.orderBy(data, ['moving_change'], ['asc']);
    roughest_data = roughest_data.slice(0,40)

    display_cols = ['Season','Team','Average Positions Lost per Season']
    columns = ['Season','team','moving_change']

    roughest_color = d3.scaleLinear()
        .domain([-5,-7.5])
        .range(["#ffffff","#ff4500"]);

    //// append the header row
    thead.append('tr')
      .selectAll('th')
      .data(display_cols).enter()
      .append('th')
        .text(function (column) { return column; });

    // create a row for each object in the data
    var rows = tbody.selectAll('tr')
      .data(roughest_data)
      .enter()
      .append('tr')
      .on("click", function(d) { 
        element = document.getElementById('team');
        element.value = d.team;
        plotResults(d.team,+d.Season,"red");
        clubDescription(d.team)
        rows.classed("highlight", false);
        d3.select(this).classed("highlight", true); })
      //.on("click", function(d) { updateFilter(d.team); })

    rows.exit().remove();

    cells = rows.selectAll('td')
        .data(function (row) {
            return columns.map(function (column) {
                return {column: column, value: row[column]};
            });
        })
        .enter()
        .append('td')
        .style("background-color", function(d){ if(d.column == "moving_change") return roughest_color(+d.value);})
        .text(function (d) { return d.value; });

    cells.exit().remove();    



})
