// Semantic form Validation
function validateSemanticForm(form,list) {
  var $form = $('#'+form); 
  if ($form.form('is valid')) return $form.form('get values', list);
  return false;
}