shinyjs.init = function() {
  $("#matchingVarsOutput").on("click", ".removeRowBtn", function(event) {
    var rowNum = $(event.target).closest(".matchParamsRow").data("row-num");
    Shiny.onInputChange("deleteRow", [rowNum, Math.random()]);
  });
};
