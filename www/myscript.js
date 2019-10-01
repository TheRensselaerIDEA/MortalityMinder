$(document).ready(() => {
  $(".sd").hide();
  
  $(".header").on("click", () => {
    $(".header_drop").slideToggle(200);
  });
  
  $(".helper_button").on("click", () => {
    $(".helper").toggleClass("helper_active",350);
  });
  
  $(".correlation").on("click", () => {
    $(".mort_ana").hide();
    $(".menu").hide();
    $(".sd").show();
  });
  
  $('.mort_trend').on("click", () => {
    $(".sd").hide();
    $(".menu").hide();
    $(".mort_ana").show();
  });
  
  $(".draggble").draggable({
    containment : 'parent',
    cursor : 'grabbing',
    opacity : 0.5,
    snap:true,
    snapTolenrance: 10,
    grid:[200,200]
  });
  
  
//  $('html').click(function() {
//    $(".menu").hide();
//  });

//  $('.menu').click(function(event){
//    event.stopPropagation();
//  });
  
//  $('.menu').bind('clickoutside', function (event) {
//    $(this).hide();
//  });
  
  $(".main").on("click", () => {
    $(".header_drop").slideUp(200);
    $(".helper").removeClass("helper_active",350);
  });
  
  $(".close").on("click", () => {
    $(".menu-container").hide();
    $(".menu-background").hide();
  })
  
  $(".menu-background").on("click",() => {
    $(".menu-container").hide();
    $(".menu-background").hide();
  })
  
  $(".menu_button").on("click", () => {
    $(".menu-background").show();
    $(".menu-container").show();
  });
  
//  $(".left").on("click", () => {
//    $(".col3").animate({width: 'toggle'});
//    var toggleWidth = $(".col1").width() == Math.round($('.col1').parent().width() * 0.7) ? "40%" : "70%";
//    $(".col1").animate({ width: toggleWidth });
//  });
  
  $('#plot1').on("mouseenter", () => {
    $('.ul').css({
      borderStyle: 'groove'
    });
  });
  
  $('#plot1').on("mouseleave", () => {
    $('.ul').css({
      borderStyle: 'hidden'
    });
  });
});
