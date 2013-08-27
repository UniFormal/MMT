$(function () {
  $('#go').click(function () {
    // get the current time from the server
    $.get('/currenttime', function(time) {
      $('#time').html(new Date(parseInt(time)).toLocaleString());
    });
  });
})
