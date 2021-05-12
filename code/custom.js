/*Shiny custom handler for brush div resize*/
Shiny.addCustomMessageHandler('plot_brush_minimize',
  function(x) {
     document.getElementById(x.id).style.height = 0;
     document.getElementById(x.id).style.width = 0;
});
