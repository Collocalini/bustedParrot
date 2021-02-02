
function savePage() {
   var anchor = document.getElementById("save");
   var h = document.documentElement.outerHTML;
   anchor.setAttribute('download', 'save.html');
   anchor.setAttribute('href', 'data:text/html;charset=UTF-8,' + encodeURIComponent(h));
   }
function randomItem(items) { 
   return items[Math.floor((items.length * Math.random()))];
   }
function shuffle(array) {
  var currentIndex = array.length, temporaryValue, randomIndex;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {

    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}   

function myF(e){
   alert("myF " + e.target.innerHTML);
}

function replaceInnerHtmlIfHasGivenValueAttr(name,repl,node){
  var n = node;
  //console.log('node: ', n.outerHTML);
  if(node.getAttribute("value")==name){
    n.innerHTML = repl;
    //alert("replacing " + name);
  }
  
  return n;

}
  
function setOverrideNextToThisBranch(e){
  //alert("myF " + e.target.innerHTML);
  //alert("ThisBranch " + );
  var branchName = e.target.parentNode.getAttribute("value").split("option_")[1];
  //document.getElementById("cache").children = 
  ([].slice.call(document.getElementById("cache").children)).map(function(n){
    return replaceInnerHtmlIfHasGivenValueAttr("overrideNxt1"
      ,"<param value=\"" + branchName + "\">"
      ,n
      );
    }
    );

}
   
