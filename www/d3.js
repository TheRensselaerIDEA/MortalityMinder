//Author: Ziyi

var width = 400,
     height = 600;
var root = svg
  .attr("width", "100%")
  .attr("height", "100%")
  .append("g");
  
function sequenceMap(index,tooltip) {
    var death_rate_domain = [0,10,20,30,40,50,60,70,80,90,100,110,120];
    var color = d3.scale.threshold()
            .domain(death_rate_domain)
            .range(["#FFFFFF", "#FFD8D8", "#FFAFAF", "#FF8888", "#FF5F5F", "#FF3838",
                    "#FF0F0F", "#D80000", "#AF0000", "#880000", "#5F0000", "#380000"]);
    svg.selectAll('.state').transition()  //select all the state element and prepare for a transition to new values
      .duration(750)  // give it a smooth time period for the transition
      .attr('fill', function(d) {
        return color(d.properties.death_rate[index]);  // the end color value
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
  var state = data[2];
  var death_rate_domain = [0,10,20,30,40,50,60,70,80,90,100,110,120];
  var death_rate = d3.map();
  var color = d3.scale.threshold()
              .domain(death_rate_domain)
              .range(["#FFFFFF", "#FFD8D8", "#FFAFAF", "#FF8888", "#FF5F5F", "#FF3838",
                      "#FF0F0F", "#D80000", "#AF0000", "#880000", "#5F0000", "#380000"]);
  
              
  var tooltip = d3.select("body").append("div") 
        .attr("class", "tooltip")       
        .style("opacity", 0);
  root.attr("transform", "scale(" + 1.5 * width / 900 + ")");
  
  
  var geo = topojson.feature(us, us.objects.collection);
  //only newyork state data
  var geo2 = geo.features.filter(function(d){
    return d.properties.state == state;
  });
  
  path = d3.geo.path();
  for(var i = 0; i < stat.length; i++){
    var dataFips = stat[i].county_fips;
	// Grab data value 
	 var dataValue = [stat[i]["2000-2002"],stat[i]["2003-2005"],stat[i]["2006-2008"],
	 stat[i]["2009-2011"],stat[i]["2012-2014"],stat[i]["2015-2017"]];
	 for(var j = 0; j < geo2.length; j++){
	   var jsonFips = geo2[j].properties.fips
	   if (dataFips == jsonFips) {
	     geo2[j].properties.death_rate = dataValue;
	     break;
	   }
	 }
  }    
  
  //map      
  root.selectAll(".state")
      .data(geo2)
      .enter().append("path")
      .attr("class", "state")
      .attr("d", path)
      .attr("fill",function(d){
        return color(d.properties.death_rate[0]);
      })
      .attr("transform","translate(-300,0)")
      .on("mouseover", function(d) {    
            tooltip.transition()    
            .duration(200)    
            .style("opacity", .9);    
            tooltip.html(d.properties.name + "\n" + d.properties.death_rate[0])  
            .style("left", (d3.event.pageX) + "px")   
            .style("top", (d3.event.pageY - 28) + "px");  
          })          
          .on("mouseout", function(d) {   
            tooltip.transition()    
            .duration(500)    
            .style("opacity", 0); 
          });
          
          
  //title
  var text = svg
            .attr("width", "100%")
            .attr("height", "100%")
  text.append("text")
      .attr("x", 300)
      .attr("y", 100)
      .attr("text-anchor", "middle")  
      .style("font-size", "8px") 
      .style("text-decoration", "underline")  
      .text(state + " Death of despair 2000-2002");
          
  var rect = svg
            .attr("width", "100%")
            .attr("height", "100%")
  //color scale        
  rect.selectAll("rect")
          .data(death_rate_domain)
          .enter()
          .append("rect")
          .attr({
              width: 10,
              height: 5,
              y: 50,
              x: function (d, i) {
                  return 10 * i;
              },
              fill: color
          });
          
  var index = 0;        
  var timer;
  var playing = false;
    d3.select('#play')  
    .on('click', function() {  // when user clicks the play button
      if(playing == false) {  // if the map is currently playing
      console.log("asd");
        timer = setInterval(function(){   // set a JS interval
          if(index < 5) {  
              index += 1;  // increment the current attribute counter
          } else {
              index = 0;  // or reset it to zero
          }
          sequenceMap(index,tooltip);  // update the representation of the map 
          d3.select('#clock').html(index);  // update the clock
        }, 2000);
        d3.select(this).html('stop');  // change the button label to stop
        playing = true;   // change the status of the animation
      } else {    // else if is currently playing
        clearInterval(timer);   // stop the animation by clearing the interval
        d3.select(this).html('play');   // change the button label to play
        playing = false;   // change the status again
      }
  });
});