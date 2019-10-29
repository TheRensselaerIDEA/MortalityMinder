$(document).ready(() => {
var image = document.getElementById("national_map");
var btn = document.getElementById("play");
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
function changeImage(){
    if(playing === true){
      if(position >= images.length){
        position = 0;
      }
      image.src = images[position];
      position++;
    }
}
setInterval(changeImage, 1000);
});