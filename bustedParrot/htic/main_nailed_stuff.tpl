
<script>

function confirmGoTo(msg , dst) {
  if (confirm(msg) == true) {
    window.location.href = dst;
  }
}

function confirmGoToNeocitiesMirror(dstConfirmGoToNeocitiesMirror){
  msgConfirmGoToNeocitiesMirror = "Перейти на зеркало на Neocities / go to Neocities mirror?";
  //dstConfirmGoToNeocitiesMirror = ${neocitiesSiteMirror};
  confirmGoTo(msgConfirmGoToNeocitiesMirror, dstConfirmGoToNeocitiesMirror);
}

function confirmGoToReplitMirror(dstConfirmGoToReplitMirror){
  msgConfirmGoToReplitMirror = "Перейти на зеркало на replit / go to replit mirror?";
  //dstConfirmGoToReplitMirror = ${replitSiteMirror};
  confirmGoTo(msgConfirmGoToReplitMirror,dstConfirmGoToReplitMirror );
}


function autostart(e){
  //alert("autostart ");
  document.getElementById("display").children[1].children[0].click();
}

window.addEventListener('load', autostart, false);
 
</script>


<noscript>
<div class='nailed_stuff_h'>
<p align='center'> 
    
      <img src='${myToyBoatTop.png}' style='width:100%;height:auto;' />   
    
</p>
</div>
</noscript>

<div id="pagesTop" style="margin:2em;overflow-wrap: break-word;"></div>

<div id="display" style="position:relative;left:0em;"></div>
       
<div id="pagesBottom" style="margin:2em;overflow-wrap: break-word;"></div>
            
<data id="cache" hidden="true">
   <data id="pages"></data>
   <data value="overrideNxt1">
      <param value="branchA">
   </data>
</data>

<data id="settings" hidden="true">
   <data value="newPageThreshold">10</data>
</data>

<div hidden="true">
  <a style='margin:1em' id='save' onclick='savePage()' download='save.html' href='data:text/html;charset=UTF-8,'>click to download</a>
  <a style='margin:1em' id='clearHistory'>Clear history</a>
  <a style='margin:1em' id='updateFromDisplay'>Update from display</a>
  <a style='margin:1em' id='edit'>Edit</a>
  <a style='margin:1em' id='cancel'></a>
  
  <div hidden='true'>Keep old<input checked='true' style='margin-left:1em' id='editKeepOld' type='checkbox' /></div>
  <div hidden='true'>Redirect links to new name<input style='margin-left:1em' id='editRedirectLinks' type='checkbox' /></div>
  <button id='editDelete' hidden='true'>Delete branch</button>
  <div hidden='true'>Remove links<input style='margin-left:1em' id='editRemoveLinks' type='checkbox' /></div>
  <button id='editListBranches' hidden='true'>List all branches</button>
</div>

<div id='listOfAllBranches'></div>

<data id="data" hidden="true"></data>

