Shiny.addCustomMessageHandler("sanofi_alert",
  function(x) {
    alert(x.message);
});

Shiny.addCustomMessageHandler("sanofi_disable_id",
  function(x) {
    document.getElementById(x.id).disabled = true;
});

Shiny.addCustomMessageHandler("sanofi_enable_id",
  function(x) {
    var el = document.getElementById(x.id)
    if(el !== null) {
      el.disabled = false;
    }
});

Shiny.addCustomMessageHandler("sanofi_display_id",
  function(x) {
     document.getElementById(x.id).style.display = x.display;
});

Shiny.addCustomMessageHandler("sanofi_display_class",
  function(x) {
    var nodeList = document.getElementsByClassName(x.class);
    for(i = 0; i < nodeList.length; i++) {
      nodeList[i].style.display = x.display;
    }
});

Shiny.addCustomMessageHandler("sanofi_visibility_id",
  function(x) {
     document.getElementById(x.id).style.visibility = x.visibility;
});

Shiny.addCustomMessageHandler("sanofi_visibility_class",
  function(x) {
    var nodeList = document.getElementsByClassName(x.class);
    for(i = 0; i < nodeList.length; i++) {
      nodeList[i].style.visibility = x.visibility;
    }
});

Shiny.addCustomMessageHandler("sanofi_click_id",
  function(x) {
    var element = document.getElementById(x.id);

    if(element.classList.contains("sanofi-accordion")) {
      element.firstElementChild.click();
    } else {
      element.click();
    }
});

Shiny.addCustomMessageHandler("sanofi_collapse_id",
  function(x) {

    //structure: element 1 = button and element 2 = panel
    var container = document.getElementById(x.id);
    var button = container.firstElementChild;
    var panel = button.nextElementSibling;

    panel.style.maxHeight = panel.scrollHeight + "px";

    if(button.classList.contains("sanofi-accordion-collapse")) {
      //panel is collapsed
      panel.style.maxHeight = 0;

    } else {
      //the panel is not collapsed
      panel.style.maxHeight = panel.scrollHeight + "px";

      button.classList.add("sanofi-accordion-collapse");
      panel.classList.add("sanofi-accordion-panel-collapse");

      panel.style.maxHeight = 0;
      //panel is now collapsed
    }
});

Shiny.addCustomMessageHandler("sanofi_uncollapse_id",
  function(x) {

    //structure: element 1 = button and element 2 = panel
    var container = document.getElementById(x.id);
    var button = container.firstElementChild;
    var panel = button.nextElementSibling;

    panel.style.maxHeight = panel.scrollHeight + "px";

    if(button.classList.contains("sanofi-accordion-collapse")) {
      //panel is collapsed
      panel.style.maxHeight = 0;

      button.classList.remove("sanofi-accordion-collapse");
      panel.classList.remove("sanofi-accordion-panel-collapse");

      panel.style.maxHeight = panel.scrollHeight + "px";
      //panel is now not collpased

    } else {
      //the panel is not collapsed
      panel.style.maxHeight = panel.scrollHeight + "px";
    }

});

Shiny.addCustomMessageHandler("sanofi_uncollapse_id_maxHeight",
  function(x) {

    //structure: element 1 = button and element 2 = panel
    var container = document.getElementById(x.id);
    var button = container.firstElementChild;
    var panel = button.nextElementSibling;

    panel.style.maxHeight = x.maxHeight;

    if(button.classList.contains("sanofi-accordion-collapse")) {
      //panel is collapsed
      panel.style.maxHeight = 0;

      button.classList.remove("sanofi-accordion-collapse");
      panel.classList.remove("sanofi-accordion-panel-collapse");

      panel.style.maxHeight = x.maxHeight;
      //panel is now not collpased

    } else {
      //the panel is not collapsed
      panel.style.maxHeight = x.maxHeight;
    }

});
