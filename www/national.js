$(document).ready(() => {
var image = document.getElementById("national_map_new");
var btn = document.getElementById("play");
btn.innerHTML = "Play";
var images = ["Despair/1.png","Despair/2.png","Despair/3.png",
        "Despair/4.png","Despair/5.png" ,"Despair/6.png"];
var position = 0;
var playing = true;

btn.onclick = function(){
  if(playing === true){
    playing = false;
    btn.innerHTML = "Play";
  }
  else{
    playing = true;
    btn.innerHTML = "Stop";
  }
}
var interval = setInterval(changeImage, 1000);
//different button of period
var btn1 = document.getElementById("first_period");
var btn2 = document.getElementById("second_period");
var btn3 = document.getElementById("third_period");
var btn4 = document.getElementById("forth_period");
var btn5 = document.getElementById("fifth_period");
var btn6 = document.getElementById("sixth_period");
var btn_list = [btn1,btn2,btn3,btn4,btn5,btn6];
btn1.onclick = function(){
  if(playing === true){
    playing = false;
    btn.innerHTML = "Play";
  }
  for(i = 0; i < btn_list.length; i++){
    if(i === 0){
      btn_list[i].style.backgroundColor = "red";
    }
    else{
      btn_list[i].style.backgroundColor = "white";
    }
  }
  image.src = images[0];
  position = 0;
}
btn2.onclick = function(){
  if(playing === true){
    playing = false;
    btn.innerHTML = "Play";
  }
  for(i = 0; i < btn_list.length; i++){
    if(i === 1){
      btn_list[i].style.backgroundColor = "red";
    }
    else{
      btn_list[i].style.backgroundColor = "white";
    }
  }
  image.src = images[1];
  position = 1;
}
btn3.onclick = function(){
  if(playing === true){
    playing = false;
    btn.innerHTML = "Play";
  }
  for(i = 0; i < btn_list.length; i++){
    if(i === 2){
      btn_list[i].style.backgroundColor = "red";
    }
    else{
      btn_list[i].style.backgroundColor = "white";
    }
  }
  image.src = images[2];
  position = 2;
}
btn4.onclick = function(){
  if(playing === true){
    playing = false;
    btn.innerHTML = "Play";
  }
  for(i = 0; i < btn_list.length; i++){
    if(i === 3){
      btn_list[i].style.backgroundColor = "red";
    }
    else{
      btn_list[i].style.backgroundColor = "white";
    }
  }
  image.src = images[3];
  position = 3;
}
btn5.onclick = function(){
  if(playing === true){
    playing = false;
    btn.innerHTML = "Play";
  }
  for(i = 0; i < btn_list.length; i++){
    if(i === 4){
      btn_list[i].style.backgroundColor = "red";
    }
    else{
      btn_list[i].style.backgroundColor = "white";
    }
  }
  image.src = images[4];
  position = 4;
}
btn6.onclick = function(){
  if(playing === true){
    playing = false;
    btn.innerHTML = "Play";
  }
  for(i = 0; i < btn_list.length; i++){
    if(i === 5){
      btn_list[i].style.backgroundColor = "red";
    }
    else{
      btn_list[i].style.backgroundColor = "white";
    }
  }
  image.src = images[5];
  position = 5;
}

function changeImage(){
    if(btn.innerHTML == "Stop"){
      if(position >= images.length){
        position = 0;
      }
      var i;
      for(i = 0; i < btn_list.length; i++){
        if(i === position){
          btn_list[i].style.backgroundColor = "red";
        }
        else{
          btn_list[i].style.backgroundColor = "white";
        }
      }
      image.src = images[position];
      position++;
    }
}

});