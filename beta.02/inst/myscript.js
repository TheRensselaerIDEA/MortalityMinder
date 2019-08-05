

function showGeo(geoName) {
  var i;
  var x = document.getElementsByClassName("geo");
  for (i = 0; i < x.length; i++) {
    x[i].style.visibility = "hidden"; 
  }
  document.getElementById(geoName).style.visibility = "visible"; 
}

function myfunc(div) {
  var className = div.getAttribute("class");
  if(className=="normal") {
    div.className = "active";
  }
  else{
    div.className = "normal";
  }
}

$(function() {
	$('.navbar a').bind('click',function(event){
	  event.preventDefault();
		var $anchor = $(this);
    console.log($anchor.attr('href'))
		$('html, body').stop().animate({
			scrollTop: $($anchor.attr('href')).offset().top
		}, 1000);
		event.preventDefault();
	});
});

function splitScroll() {
  const controller = new ScrollMagic.Controller();
  new ScrollMagic.Scene({
    duration:"100%",
    //triggerElement: '.parameters',
    triggerHook:0.07
  })
  .setPin('.tabs')
  //.addIndicators()
  .addTo(controller);
}

function changeImage(){
  var image = document.getElementById('mortality');
  
}


splitScroll();
