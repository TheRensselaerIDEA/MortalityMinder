//Author: Ziyi

var width = 400,
height = 600;
var root = svg
.attr("width", "100%")
.attr("height", "100%")
.append("g");

function sequenceMap(index,tooltip,cause) {
  var clusters = [1,2,3,4,5,6]
  
  var color = d3.scale.threshold()
  .domain(clusters)
  .range(["#fed976", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"]);
  svg.selectAll('.state').transition()  //select all the state element and prepare for a transition to new values
  .duration(750)  // give it a smooth time period for the transition
  .attr('fill', function(d) {
    return color(d.properties.cluster);  // the end color value
  }
  )
  svg.selectAll('.state')
  .on("mouseover", function(d) {    
    tooltip.transition()    
    .duration(200)    
    .style("opacity", .9);    
    tooltip.html(d.properties.name + "\n" + d.properties.death_rate[index])  
    .style("left", (d3.event.pageX) + "px")   
    .style("top", (d3.event.pageY - 28) + "px");  
  })          
  .on("mouseout", function(d) {   
    tooltip.transition()    
    .duration(500)    
    .style("opacity", 0); 
  });
}

d3.select("body").append("div");

r2d3.onRender(function(data, svg, width, height, options) {
  var us = data[0];
  var stat = data[1];
  var cause = data[2];
  var usa_states = data[3];
  var clusters = [1,2,3,4,5,6]
  var death_rate = d3.map();
  var color = d3.scale.threshold()
  .domain(clusters)
  .range(["#fed976", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"]);
  
  
  var tooltip = d3.select("body").append("div") 
  .attr("class", "tooltip")       
  .style("opacity", 0);
  
  var geo = topojson.feature(us, us.objects.collection);
  //only newyork state data
  var geo2 = geo.features;
  
  path = d3.geo.path();
  for(var i = 0; i < stat.length; i++){
    var dataFips = stat[i].county_fips;
    // Grab data value 
    var dataValue = [stat[i]["cluster"]];
    for(var j = 0; j < geo2.length; j++){
      var jsonFips = geo2[j].properties.fips
      if (dataFips == jsonFips) {
        geo2[j].properties.cluster = dataValue;
        break;
      }
    }
  }    
  
  //map      
  root.append("g")
  .attr("id", "counties")
  .selectAll(".state")
  .data(geo2)
  .enter().append("path")
  .attr("class", "counties")
  .attr("d", path)
  .attr("fill",function(d){
    return color(d.properties.cluster);
  })
  .on("mouseover", function(d) {    //the hovering effect
    tooltip.transition()    
    .duration(200)    
    .style("opacity", .9);    
    tooltip.html(d.properties.name + "\n" + d.properties.cluster)  
    .style("left", (d3.event.pageX) + "px")   
    .style("top", (d3.event.pageY - 28) + "px");  
  })          
  .on("mouseout", function(d) {   
    tooltip.transition()    
    .duration(500)    
    .style("opacity", 0); 
  });
  
  root.append("g")
  .attr("id", "states")
  .selectAll(".state")
  .data(topojson.feature(usa_states, usa_states.objects.us).features)
  .enter().append("path")
  .attr("class", "stateoverlay")
  .attr("d", path);

  var rect = svg
  .attr("width", "100%")
  .attr("height", "100%")
  //color scale        
  rect.selectAll("rect")
  .data(clusters)
  .enter()
  .append("rect")
  .attr({
    width: 24,
    height: 5,
    y: 520,
    x: function (d, i) {
      return 550 + 25 * i;
    },
    fill: color
  });
  
  svg.selectAll("text")
  .data(clusters)
  .enter()
  .append("text")
  .attr({
    y:520,
    x:function(d,i){
      return 550 + 25 * i;
    }})
  .text(function(d){
    return d;
  })
  ;
  
  var index = 0;        
  var timer;
  var playing = false;
  var year;
  var year2;
  var ten_year;
  var ten_year2;
  setInterval(function(){   // set a JS interval
    if(index < 5) {  
      index += 1;  // increment the current attribute counter
    } else {
      index = 0;  // or reset it to zero
    }
    sequenceMap(index,tooltip,cause);
    
  }, 2000);
});