document.addEventListener('DOMContentLoaded', loadWindow, false)

 ws = new WebSocket("ws://127.0.0.1:80/ws")
 console.log("initialized websocket")

ws.onmessage = function(evt) {   
  const d = JSON.parse(evt.data)
  const f = JSON.parse(d)
  switch (f.type){
    case 'grid': 
      const n = f.num.toString()     
      for (var i = 0; i < n.length; i++) {
        const e = document.getElementById("cell-" + i.toString())
       // console.log("cell-" + i.toString())
        if(n[i] === '0'){
          e.value = ' '
          e.disabled = false
        }
        else{
        e.value=n[i]
        e.disabled = true
        } 
      }
      break

    case 'lock': 
      Array.from(document.getElementsByTagName('button')).forEach(function (value, i, col) {
        value.disabled = true })
      break

    case 'unlock':
      Array.from(document.getElementsByTagName('button')).forEach(function (value, i, col) {
        value.disabled = false })
      break
      
    default: 
  }

  
  //console.log(typeof(d))
  //d.forEach(element => {
  //  console.log(element)
  //}) 
 // switch (d.type){
  //  case 'greeting': console.log(d.data) 
  //                  break
  //  case 'cards': displayDeck(d.data)
  //                break
  //  case 'hide': displayBack(d.data)
  //                break
  //  default: 
  //console.log(d.num)
  
   

 // }
 }

 ws.onopen = function() {
     console.log("connected")
 }

 ws.onclose = function() {
     console.log("closed websocket")
     ws.send("close")
 }

 function sleep(milliseconds) {
    var start = new Date().getTime();
    for (var i = 0; i < 1e7; i++) {
      if ((new Date().getTime() - start) > milliseconds){
        break;
      }
    }
  }

function displayDeck(arry){
    arry.forEach((value, index) => {
        document.getElementById('c' + index.toString()).src='./assets/images/' + value + '.gif'
    })
}

function displayBack(arry){
  arry.forEach((value, index) => {
      document.getElementById('c' + index.toString()).src='./assets/images/back.gif'
  })
}

function n(num, len = 2) {
    return `${num}`.padStart(len, '0');
}

function shuffle(array) {
var m = array.length, t, i;

// While there remain elements to shuffle…
while (m) {

    // Pick a remaining element…
    i = Math.floor(Math.random() * m--);

    // And swap it with the current element.
    t = array[m];
    array[m] = array[i];
    array[i] = t;
}
return array;
}

function mode (btnID) {
   ws.send(btnID + "\n") //+ "\r\n"
  
   //alert(btnID)
}


function loadWindow (){


  Array.from(document.getElementsByTagName('button')).forEach(function (value, i, col) {
    col[i].onclick = function (e) { mode(e.target.id) }
  })
}
