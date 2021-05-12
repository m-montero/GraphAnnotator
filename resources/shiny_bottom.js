/*In the navigation bar, float selected tab panels to the right*/
var sanofiRightTabPanel = $('a[data-value^="sanofiRightTabPanel"]');
if( sanofiRightTabPanel.length > 0 ) {
  sanofiRightTabPanel.each(function () {
        $( this ).parent().addClass('sanofi-tabPanel-right');
   });
}

/*Sanofi accordion (button and panel)*/
var sanofiAccordionButtonGroup = document.getElementsByClassName("sanofi-accordion-button");
var sanofiAccordionButton;
for (sanofiAccordionButton = 0; sanofiAccordionButton < sanofiAccordionButtonGroup.length; sanofiAccordionButton++) {
  sanofiAccordionButtonGroup[sanofiAccordionButton].addEventListener("click", function() {

    //structure: element 1 = button and element 2 = panel
    var panel = this.nextElementSibling;

    panel.style.maxHeight = panel.scrollHeight + "px";

    if(this.classList.contains("sanofi-accordion-collapse")) {
      //panel is collapsed
      panel.style.maxHeight = 0;

      this.classList.remove("sanofi-accordion-collapse");
      panel.classList.remove("sanofi-accordion-panel-collapse")

      panel.style.maxHeight = panel.scrollHeight + "px";
      //panel is now not collpased

    } else {
      //the panel is not collapsed
      panel.style.maxHeight = panel.scrollHeight + "px";

      this.classList.add("sanofi-accordion-collapse");
      panel.classList.add("sanofi-accordion-panel-collapse");

      panel.style.maxHeight = 0;
      //panel is now collapsed
    }

  });
}
