function bsSizeCategory(wid) {
  if (wid <= 576) return 1;
  if (wid <= 768) return 2;
  if (wid <= 992) return 2;
  return 4;
}

var dimension = [0, 0];

$(document).on("shiny:connected", function(e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  
  window.currentCategory = bsSizeCategory(window.innerWidth);
  Shiny.setInputValue("width", window.currentCategory);
});

$(window).resize(function(e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  
  if (bsSizeCategory(window.innerWidth) != window.currentCategory) {
    window.currentCategory = bsSizeCategory(window.innerWidth);
    Shiny.setInputValue("width", window.currentCategory);
  }
});
