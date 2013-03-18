function toggleSpecies(){
	var selected_species = $('input[name="species"]:checked').val();
    $('[name="cmaps"][species=' + selected_species + ']').removeAttr('disabled');
    $('[name="cmaps"][species=' + selected_species + ']').show();
    $('[name="cmaps"][species!=' + selected_species + ']').prop('checked',false);
    $('[name="cmaps"][species!=' + selected_species + ']').prop('disabled',true);
    $('[name="cmaps"][species!=' + selected_species + ']').hide();
    togglePlatforms();
};

function togglePlatforms(){
  if( $('input[name="idType"]:checked').val() == "probe") {
  	var selected_species = $('input[name="species"]:checked').val();
      $("#platform_request").show();
      $("#platform_request").removeAttr('disabled');
      $('[name="platform"][species=' + selected_species + ']').show();
      $('[name="platform"][species=' + selected_species + ']').removeAttr('disabled');
      $('[name="platform"][species!=' + selected_species + ']').hide();
      $('[name="platform"][species!=' + selected_species + ']').prop('disabled',true);
  } else {
    $("#platform_request").hide();
    $('[name="platform"]').hide();
    $("#platform_request").prop('disabled',true);
    $('[name="platform"]').prop('disabled',true);
  }
};

// concatenation of multiple checkbox choices

function combine_checkBoxes( form, element ){
  var result, vals = [], checkBoxName = document.forms[ form ][element ];
  for(var i=0,elm;elm = checkBoxName[i];i++) {
    if(elm.checked) {
      vals.push(elm.value);
    }
  }
  result = vals.join('++');
  return result
}

jQuery.validator.addMethod("require_from_group", function(value, element, options) {
    var valid = $(options[1], element.form).filter(function() {
        return $(this).val(); asd
    }).length >= options[0];
    
    if(!$(element).data('reval')) {
        var fields = $(options[1], element.form);
        fields.data('reval', true).valid();
        fields.data('reval', false);
    }
    return valid;
});


// input field validation
function submit_form()
{
  $("#query_form").validate
  (
  { 
    rules:{
      query_data: {
        require_from_group: [1,".user_input"]
      },
      query_data_up: {
        require_from_group: [1,".user_input"]
      },
      cmaps: {
        "required": true,
        "minlength": 1
      },
    },
    messages:{
      cmaps:"Please choose at least one reference dataset.",
      query_data: "Please paste or upload your data.",
      query_data_up: "Please provide gene identifiers for at least one category (up- and/or down).",
    },

    errorClass: "alert",
    onfocusout: false,
    errorLabelContainer: "#error_messages",

    highlight: function(element, errorClass, validClass)
    {
      $(element).parents('.control-group').addClass('error');
    },

    unhighlight: function(element, errorClass, validClass)
    {
      $(element).parents('.control-group').removeClass('error');
    },

    // if validated, submit form via ajax post
    submitHandler: function( form )
    {
      // toggle submit button
      $('#submitButtonId').button('loading')

       // concatenate checkbox choices
       a = _.collect($(form).find('input[name=cmaps]:checked'), function(el){ return $(el).val()}).join(',')
       $(form).find('input[name=selected_cmaps]').val(a) // set selected_cmaps field
       $(form).find('input[name=cmaps]').attr('disabled','disabled') // temporarily disable single cmaps
       // report successful submission
       $('#feedback').append("<div class=\"alert alert-success\">Your query has been submitted! Please be patient, some queries can take several minutes to process...</div>");
       form.submit()
   }
 }
 );
 // re-enable single cmaps
 $('#query_form').find('input[name=cmaps]').removeAttr('disabled');
 // toggle submit button
 $('#submitButtonId').button('reset');
};

// integrate dataTable classes with bootstrap layout
/* Set the defaults for DataTables initialisation */
$.extend( true, $.fn.dataTable.defaults, {
	"sDom": "<'row-fluid'<'span6'l><'span6'f>r>t<'row-fluid'<'span6'i><'span6'p>>",
	"sPaginationType": "bootstrap",
	"oLanguage": {
		"sLengthMenu": "_MENU_ records per page"
	}
} );


/* Default class modification */
$.extend( $.fn.dataTableExt.oStdClasses, {
	"sWrapper": "dataTables_wrapper form-inline"
} );


/* API method to get paging information */
$.fn.dataTableExt.oApi.fnPagingInfo = function ( oSettings )
{
	return {
		"iStart":         oSettings._iDisplayStart,
		"iEnd":           oSettings.fnDisplayEnd(),
		"iLength":        oSettings._iDisplayLength,
		"iTotal":         oSettings.fnRecordsTotal(),
		"iFilteredTotal": oSettings.fnRecordsDisplay(),
		"iPage":          Math.ceil( oSettings._iDisplayStart / oSettings._iDisplayLength ),
		"iTotalPages":    Math.ceil( oSettings.fnRecordsDisplay() / oSettings._iDisplayLength )
	};
};


/* Bootstrap style pagination control */
$.extend( $.fn.dataTableExt.oPagination, {
	"bootstrap": {
		"fnInit": function( oSettings, nPaging, fnDraw ) {
			var oLang = oSettings.oLanguage.oPaginate;
			var fnClickHandler = function ( e ) {
				e.preventDefault();
				if ( oSettings.oApi._fnPageChange(oSettings, e.data.action) ) {
					fnDraw( oSettings );
				}
			};

			$(nPaging).addClass('pagination').append(
				'<ul>'+
					'<li class="prev disabled"><a href="#">&larr; '+oLang.sPrevious+'</a></li>'+
					'<li class="next disabled"><a href="#">'+oLang.sNext+' &rarr; </a></li>'+
				'</ul>'
			);
			var els = $('a', nPaging);
			$(els[0]).bind( 'click.DT', { action: "previous" }, fnClickHandler );
			$(els[1]).bind( 'click.DT', { action: "next" }, fnClickHandler );
		},

		"fnUpdate": function ( oSettings, fnDraw ) {
			var iListLength = 5;
			var oPaging = oSettings.oInstance.fnPagingInfo();
			var an = oSettings.aanFeatures.p;
			var i, j, sClass, iStart, iEnd, iHalf=Math.floor(iListLength/2);

			if ( oPaging.iTotalPages < iListLength) {
				iStart = 1;
				iEnd = oPaging.iTotalPages;
			}
			else if ( oPaging.iPage <= iHalf ) {
				iStart = 1;
				iEnd = iListLength;
			} else if ( oPaging.iPage >= (oPaging.iTotalPages-iHalf) ) {
				iStart = oPaging.iTotalPages - iListLength + 1;
				iEnd = oPaging.iTotalPages;
			} else {
				iStart = oPaging.iPage - iHalf + 1;
				iEnd = iStart + iListLength - 1;
			}

			for ( i=0, iLen=an.length ; i<iLen ; i++ ) {
				// Remove the middle elements
				$('li:gt(0)', an[i]).filter(':not(:last)').remove();

				// Add the new list items and their event handlers
				for ( j=iStart ; j<=iEnd ; j++ ) {
					sClass = (j==oPaging.iPage+1) ? 'class="active"' : '';
					$('<li '+sClass+'><a href="#">'+j+'</a></li>')
						.insertBefore( $('li:last', an[i])[0] )
						.bind('click', function (e) {
							e.preventDefault();
							oSettings._iDisplayStart = (parseInt($('a', this).text(),10)-1) * oPaging.iLength;
							fnDraw( oSettings );
						} );
				}

				// Add / remove disabled classes from the static elements
				if ( oPaging.iPage === 0 ) {
					$('li:first', an[i]).addClass('disabled');
				} else {
					$('li:first', an[i]).removeClass('disabled');
				}

				if ( oPaging.iPage === oPaging.iTotalPages-1 || oPaging.iTotalPages === 0 ) {
					$('li:last', an[i]).addClass('disabled');
				} else {
					$('li:last', an[i]).removeClass('disabled');
				}
			}
		}
	}
} );


/*
 * TableTools Bootstrap compatibility
 * Required TableTools 2.1+
 */
if ( $.fn.DataTable.TableTools ) {
	// Set the classes that TableTools uses to something suitable for Bootstrap
	$.extend( true, $.fn.DataTable.TableTools.classes, {
		"container": "DTTT btn-group",
		"buttons": {
			"normal": "btn",
			"disabled": "disabled"
		},
		"collection": {
			"container": "DTTT_dropdown dropdown-menu",
			"buttons": {
				"normal": "",
				"disabled": "disabled"
			}
		},
		"print": {
			"info": "DTTT_print_info modal"
		},
		"select": {
			"row": "active"
		}
	} );

	// Have the collection use a bootstrap compatible dropdown
	$.extend( true, $.fn.DataTable.TableTools.DEFAULTS.oTags, {
		"collection": {
			"container": "ul",
			"button": "li",
			"liner": "a"
		}
	} );
}


/* Table initialisation */
$(document).ready(function() {
	$('#example').dataTable( {
		"sDom": "<'row'<'span6'l><'span6'f>r>t<'row'<'span6'i><'span6'p>>",
		"sPaginationType": "bootstrap",
		"oLanguage": {
			"sLengthMenu": "_MENU_ records per page"
		}
	} );
} );
