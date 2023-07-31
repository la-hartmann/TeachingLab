$(document).ready(function() {

  /*$chunks = $('.fold');

  $chunks.each(function () {

    // add button to source code chunks
    if ( $(this).hasClass('s') ) {
      $('pre.r', this).prepend("<div class=\"showopt\">Show Source</div><br style=\"line-height:22px;\"/>");
      $('pre.r', this).children('code').attr('class', 'folded');
    }

    // add button to output chunks
    if ( $(this).hasClass('o') ) {
      $('pre:not(.r)', this).has('code').prepend("<div class=\"showopt\">Show Output</div><br style=\"line-height:22px;\"/>");
      $('pre:not(.r)', this).children('code:not(r)').addClass('folded');

      // add button to plots
      $(this).find('img').wrap('<pre class=\"plot\"></pre>');
      $('pre.plot', this).prepend("<div class=\"showopt\">Show</div><br style=\"line-height:22px;\"/>");
      $('pre.plot', this).children('img').addClass('folded');

    }
  });

  // hide all chunks when document is loaded
  $('.folded').css('display', 'none')

  // function to toggle the visibility
  $('.showopt').click(function() {
    var label = $(this).html();
    if (label.indexOf("Show") >= 0) {
      $(this).html(label.replace("Show", "Hide"));
    } else {
      $(this).html(label.replace("Hide", "Show"));
    }
    $(this).siblings('code, img').slideToggle('fast', 'swing');
  });*/
  $plots = $('img.plot');
  $chunks = $('pre').has('code');
  $chunks = $chunks.filter(function(idx) {
    return $(this).children('code').outerHeight(false) > parseInt($(this).css('line-height'));
  });

  $chunks.each(function () {
    if($(this).hasClass('r')) {
      $(this).append("<div class=\"showopt\">Show Source</div><br style=\"line-height:22px;\"/>");
    } else {
      $(this).append("<div class=\"showopt\">Show Output</div><br style=\"line-height:22px;\"/>");
    }
  });

  $plots.each(function () {
    $(this).wrap('<pre class=\"plot\"></pre>');
    $(this).parent('pre.plot').prepend("<div class=\"showopt\">Show</div><br style=\"line-height:22px;\"/>");
  });

  // hide all chunks when document is loaded
  $chunks.children('code').toggle();
  $('pre.plot').children('img').toggle();
  // function to toggle the visibility
  $('.showopt').click(function() {
    var label = $(this).html();
    if (label.indexOf("Show") >= 0) {
      $(this).html(label.replace("Show", "Hide"));
    } else {
      $(this).html(label.replace("Hide", "Show"));
    }
    $(this).siblings('code, img').slideToggle('fast', 'swing');
  });
  
  // Make mouse over function work
  $('#explanation').mouseover(function() {
    $("#info").css("background-color", "yellow");
    $("#info").css("color", "black");
    $("#info").css("display", "block");
    // $('#info').style.display = 'block';
    //document.getElementById('info').style.display = 'block'; 
  });
    
  $('#explanation').mouseout(function() {
    $("#info").css("background-color", "white");
    $("#info").css("display", "none");
    // $('#info').style.display = 'none';
    //document.getElementById('info').style.display = 'none'; 
  });
  
  
});
