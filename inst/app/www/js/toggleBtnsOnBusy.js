// code from
// https://stackoverflow.com/questions/48851729/how-can-i-disable-all-action-buttons-while-shiny-is-busy-and-loading-text-is-dis

$(document).on("shiny:busy", function() {
  var inputs = document.getElementsByTagName("button");
  console.log(inputs);
for (var i = 0; i < inputs.length; i++) {
inputs[i].disabled = true;
}
});

$(document).on("shiny:idle", function() {
  var inputs = document.getElementsByTagName("button");
  console.log(inputs);
for (var i = 0; i < inputs.length; i++) {
inputs[i].disabled = false;
}
});
