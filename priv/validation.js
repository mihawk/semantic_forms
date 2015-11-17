// Semantic form Validation
function validateSemanticForm(form,list) {
  var $form = $('#'+form); 
  //console.log($form.form('get values',list));
  if (form == 'startBid') return $form.form('get values', list); 
  	else if ($form.form('is valid')) return $form.form('get values', list);
  return false;
  //return $form.form('get values',list);
}