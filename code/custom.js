/*Shiny custom handler for brush div resize*/
Shiny.addCustomMessageHandler('plot_brush_minimize',
  function(x) {
     document.getElementById(x.id).style.height = 0;
     document.getElementById(x.id).style.width = 0;
});

/*function to assign css variable vw value; i.e. viewWidth*/
function setPlotViewWidth() {
  if(window.innerWidth < 2100){
    let vw = window.innerWidth * 0.01;
    document.documentElement.style.setProperty('--vw', `${vw}px`);
  } else if(window.innerWidth < 2800){
    let vw = window.innerWidth * 0.0075;
    document.documentElement.style.setProperty('--vw', `${vw}px`);
  } else if(window.innerWidth < 4100){
    let vw = window.innerWidth * 0.005;
    document.documentElement.style.setProperty('--vw', `${vw}px`);
  }  else if(window.innerWidth < 5500){
    let vw = window.innerWidth * 0.00375;
    document.documentElement.style.setProperty('--vw', `${vw}px`);
  }
}

/*Initial sizing without resize event listener*/
$(document).on('shiny:connected', function(event) {
  setPlotViewWidth();
});


/*Set max width for plotTab2 based on window Size*/
$(document).on('shiny:connected', function(event) {
  window.addEventListener('resize', () => {
    if(Shiny.shinyapp.$inputValues.tabs_top_header == "tab2_Visualize"){
      setPlotViewWidth();
    }
  });
});

$(document).on('shiny:inputchanged', function(event) {
  if(event.name === 'tabs_top_header'){
    setPlotViewWidth();
  }
});
