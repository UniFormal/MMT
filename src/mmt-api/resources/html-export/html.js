function toggle(elem,s) {
   $(elem).closest('.constant').find('.' + s).toggle()
}

var JOBAD1;
$(function(){	
  JOBAD1 = new JOBAD($("math"));
  JOBAD1.modules.load("kwarc.mmt.navigation", []);
  JOBAD1.modules.load("kwarc.mmt.hovering", []);
  JOBAD1.modules.load("kwarc.mmt.intvw", []);
  JOBAD1.Setup();
});		
