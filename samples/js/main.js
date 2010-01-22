//dialog, rounded corners, simpletooltip and invite stuff lives here

//invite functions
	var invites = 3;
function addInvite(hasRemove, inviteType){
	if(inviteType == 2){
		var inviteText = '<div id="number'+invites+'SMS"><label class="inviteLabel">Phone Number</label><input class="inviteInput" type="text" /> 	 &nbsp;<img src="images/addInvite.gif" onClick="addInvite(1,2)" id="add" class="hover" title="Add Another Friend" /> ';
		if(hasRemove){
			inviteText += '<img src="images/removeInvite.gif" onClick="removeInvite(this.id)" class="hover remove" title="Remove Row" id="number'+invites+'SMS" />';
		}
		inviteText += '</div>';
		$('.SMS.invitees').append(inviteText);
	}
	else{
		var inviteText = '<div id="number'+invites+'email"><label class="inviteLabel">Friend\'s Email</label><input class="inviteInput" type="text" /> 	 &nbsp;<img src="images/addInvite.gif" onClick="addInvite(1,3)" id="add" class="hover" title="Add Another Friend" /> ';
		if(hasRemove){
			inviteText += '<img src="images/removeInvite.gif" onClick="removeInvite(this.id)" class="hover remove" title="Remove Row" id="number'+invites+'email" />';
		}
		inviteText += '</div>';
		$('.email.invitees').append(inviteText);
	}
	invites ++;
	$(".hover").simpletooltip();
	//numInvites++;
	//$("#numFriends").text(numInvites)
}
function removeInvite(removeID){
	$('#'+removeID).hide();
	//numInvites--;
	//$("#numFriends").text(numInvites)
}
var invite = {
	func: function(){
		invites = 2;
		numInvites = 0;
		addInvite(0,0);
	}
};
var legal = {
	func: function(){
		$("#scroll").click(function(){
			$('.dialogContent').animate({
			scrollTop: $("#"+$(this).attr('class')).offset().top
			},1000);
		});
	}
};
//corner settings
$(function(){ 
	settings = {
		tl: { radius: 10 },
		tr: { radius: 10 },
		bl: { radius: 10 },
		br: { radius: 10 },
		antiAlias: true,
		autoPad: true,
		validTags: ["div"]
	}

	dialog = {
		tl: { radius: 5 },
		tr: { radius: 5 },
		bl: { radius: 0 },
		br: { radius: 0 },
		antiAlias: true,
		autoPad: true,
		validTags: ["div"]
	}
});
$(document).ready(function(){
	$('a').click(function(){
		if($(this).attr('href') == '#'){
			return false;
		}
	});
	$('.invitePop').click(function(){
		loadPopup(this, invite, 620, 250);
		return false;
	});
	$(".hover").simpletooltip();
	$('.footerHolder').hover(function(){
		$('.footerHolder img').attr('src', 'images/spreezioBottomOver.gif');
		$(this).addClass('footerOver');
	},function(){
		$('.footerHolder img').attr('src', 'images/spreezioBottom.gif');
		$(this).removeClass('footerOver');
	});
	$.preloadImages("images/spreezioBottomOver.gif","images/footerShareOver.gif","images/footerTwitterOver.gif","images/footerFeedbackOver.gif");
});


//preload images
jQuery.preloadImages = function()
{
  for(var i = 0; i<arguments.length; i++)
  {
    jQuery("<img>").attr("src", arguments[i]);
  }
}

//Dialogs by Casey!
var popupOpen = 0;
function loadPopup(url, load, width, height){
	//loads popup only if it is disabled
	if(popupOpen==0){
		//add the background
		$(document.createElement("div")).attr("class","backgroundOverlay").prependTo ("body");
		$(document.createElement("div")).attr("class","loading").html('<img src="images/ajax-loader.gif" />').prependTo ("body");
		centerPopup('.loading');
		var arrPageSizes = getPageSize();
		// Style overlay and show it
		$('.backgroundOverlay').css({
			opacity:		0.8,
			width:			arrPageSizes[0],
			height:			arrPageSizes[1]
		}).fadeIn();
		
		$('embed, object, select').css({ 'visibility' : 'hidden' }); //hide the things that will show through
		//add the loading icon
		
		$(document.createElement("div")).attr("class","dialog").prependTo ("body").load(url.href,function(){
			$(".closePopup").click(function(){
				closePopup('.dialog');
			});
			//Click out event!
			$(".backgroundOverlay").click(function(){
				closePopup('.dialog');
			});
			
			if(width != null){
				$('.dialog').css({'width':width+"px"});
			}
			if(height != null){
				$('.dialogContent').css({'height':height+"px"});
			}
			centerPopup('.dialog');
			$('.dialog').fadeIn("normal");
			$('.loading').remove();
	  		$('.dialogTitle').corner(dialog);
			if(load.func != null && load.func != undefined){
				$(".dialog").ajaxComplete(load.func);
			}
			$('a').click(function(){
				if($(this).attr('href') == '#'){
					return false;
				}
			});
		});
		popupOpen = 1;
	}
}

function closePopup(object){
	//closes popup only if it is opened
	if(popupOpen==1){
		$(".backgroundOverlay").fadeOut("normal", function () {
		 	$(this).remove();
		});
		$(object).fadeOut("normal", function () {
		 	$(this).remove();
		});
		$('embed, object, select').css({ 'visibility' : 'visible' });
		popupOpen = 0;
	}
}
function centerPopup(object){
	//request data for centering
	var windowWidth = document.documentElement.clientWidth;
	var windowHeight = document.documentElement.clientHeight;
	var popupHeight = $(object).height();
	var popupWidth = $(object).width();
	//center
	$(object).css({
		"position": "absolute",
		"top": (windowHeight/2-popupHeight/2) + $(window).scrollTop(),
		"left": (windowWidth/2-popupWidth/2) + $(window).scrollLeft()
	});
	//only need force for IE6

}
function getPageSize() {
	var xScroll, yScroll;
	if (window.innerHeight && window.scrollMaxY) {	
		xScroll = window.innerWidth + window.scrollMaxX;
		yScroll = window.innerHeight + window.scrollMaxY;
	} else if (document.body.scrollHeight > document.body.offsetHeight){ // all but Explorer Mac
		xScroll = document.body.scrollWidth;
		yScroll = document.body.scrollHeight;
	} else { // Explorer Mac...would also work in Explorer 6 Strict, Mozilla and Safari
		xScroll = document.body.offsetWidth;
		yScroll = document.body.offsetHeight;
	}
	var windowWidth, windowHeight;
	if (self.innerHeight) {	// all except Explorer
		if(document.documentElement.clientWidth){
			windowWidth = document.documentElement.clientWidth; 
		} else {
			windowWidth = self.innerWidth;
		}
		windowHeight = self.innerHeight;
	} else if (document.documentElement && document.documentElement.clientHeight) { // Explorer 6 Strict Mode
		windowWidth = document.documentElement.clientWidth;
		windowHeight = document.documentElement.clientHeight;
	} else if (document.body) { // other Explorers
		windowWidth = document.body.clientWidth;
		windowHeight = document.body.clientHeight;
	}	
	// for small pages with total height less then height of the viewport
	if(yScroll < windowHeight){
		pageHeight = windowHeight;
	} else { 
		pageHeight = yScroll;
	}
	// for small pages with total width less then width of the viewport
	if(xScroll < windowWidth){	
		pageWidth = xScroll;		
	} else {
		pageWidth = windowWidth;
	}
	arrayPageSize = new Array(pageWidth,pageHeight,windowWidth,windowHeight);
	return arrayPageSize;
	};


/**
*	simpleTooltip jQuery plugin, by Marius ILIE
*	visit http://dev.mariusilie.net for details
**/
(function($){ $.fn.simpletooltip = function(){
	return this.each(function() {
		var text = $(this).attr("title");
		$(this).attr("title", "");
		if(text != null && text != '') {
			$(this).hover(function(e){
				var tipX = e.pageX + 12;
				var tipY = e.pageY + 12;
				$(this).attr("title", ""); 
				$("body").append("<div id='simpleTooltip' style='position: absolute; z-index: 100; display: none;'>" + text + "</div>");
				if($.browser.msie) var tipWidth = $("#simpleTooltip").outerWidth(true)
				else var tipWidth = $("#simpleTooltip").width()
				$("#simpleTooltip").width(tipWidth);
				$("#simpleTooltip").css("left", tipX).css("top", tipY).fadeIn("medium");
			}, function(){
				$("#simpleTooltip").remove();
				$(this).attr("title", text);
			});
			$(this).mousemove(function(e){
				var tipX = e.pageX + 12;
				var tipY = e.pageY + 12;
				var tipWidth = $("#simpleTooltip").outerWidth(true);
				var tipHeight = $("#simpleTooltip").outerHeight(true);
				if(tipX + tipWidth > $(window).scrollLeft() + $(window).width()) tipX = e.pageX - tipWidth;
				if($(window).height()+$(window).scrollTop() < tipY + tipHeight) tipY = e.pageY - tipHeight;
				$("#simpleTooltip").css("left", tipX).css("top", tipY).fadeIn("medium");
			});
		}
	});
}})(jQuery);

/*ie6 z-index fix: http://plugins.jquery.com/project/bgiframe*/
(function($){ $.fn.bgIframe = $.fn.bgiframe = function(s) {
	// This is only for IE6
	if ( $.browser.msie && /6.0/.test(navigator.userAgent) ) {
		s = $.extend({
			top     : 'auto', // auto == .currentStyle.borderTopWidth
			left    : 'auto', // auto == .currentStyle.borderLeftWidth
			width   : 'auto', // auto == offsetWidth
			height  : 'auto', // auto == offsetHeight
			opacity : true,
			src     : 'javascript:false;'
		}, s || {});
		var prop = function(n){return n&&n.constructor==Number?n+'px':n;},
		    html = '<iframe class="bgiframe"frameborder="0"tabindex="-1"src="'+s.src+'"'+
		               'style="display:block;position:absolute;z-index:-1;'+
			               (s.opacity !== false?'filter:Alpha(Opacity=\'0\');':'')+
					       'top:'+(s.top=='auto'?'expression(((parseInt(this.parentNode.currentStyle.borderTopWidth)||0)*-1)+\'px\')':prop(s.top))+';'+
					       'left:'+(s.left=='auto'?'expression(((parseInt(this.parentNode.currentStyle.borderLeftWidth)||0)*-1)+\'px\')':prop(s.left))+';'+
					       'width:'+(s.width=='auto'?'expression(this.parentNode.offsetWidth+\'px\')':prop(s.width))+';'+
					       'height:'+(s.height=='auto'?'expression(this.parentNode.offsetHeight+\'px\')':prop(s.height))+';'+
					'"/>';
		return this.each(function() {
			if ( $('> iframe.bgiframe', this).length == 0 )
				this.insertBefore( document.createElement(html), this.firstChild );
		});
	}
	return this;
};

})(jQuery);

/*curvy corners*/
function styleit()
{
	for(var t = 0; t < document.styleSheets.length; t++)
	{
		var theRules = new Array();
		theRules = document.styleSheets[t].rules;

		for(var i = 0; i < theRules.length; i++)
		{
		
			var allR = theRules[i].style.CCborderRadius    || 0;
			var tR   = theRules[i].style.CCborderRadiusTR  || allR;
			var tL   = theRules[i].style.CCborderRadiusTL  || allR;
			var bR   = theRules[i].style.CCborderRadiusBR  || allR;
			var bL   = theRules[i].style.CCborderRadiusBL  || allR;

			if (allR || tR || tR || bR || bL)
			{
				var s = theRules[i].selectorText;
				
				var settings = {					
					tl: { radius: makeInt(tL) },
					tr: { radius: makeInt(tR) },
					bl: { radius: makeInt(bL) },
					br: { radius: makeInt(bR) },
					antiAlias: true,
					autoPad: true,
					validTags: ["div"]
				};
				
				$(s).corner(settings); 

			}
		}
	}
}

// static class function to determine if the sheet is worth parsing
function opera_contains_border_radius(sheetnumber) {
  return /border-((top|bottom)-(left|right)-)?radius/.test(document.styleSheets.item(sheetnumber).ownerNode.text);
}

function makeInt(num) {
	var re = new RegExp('([0-9]*)');
	var i = 0;
	if(isNaN(num)) {
		var a = re.exec(num);
		if(!isNaN(parseInt(a[1]))) {
			i = a[1];
		}
	}
	else {
		i = num;
	}	
	return i;
}

(function($) { 

	$(function(){
		if ($.browser.msie) {	
			styleit();
		} else if ($.browser.opera) {

      		for (t = 0; t < document.styleSheets.length; ++t) {

        		if (opera_contains_border_radius(t)) {

					var txt = document.styleSheets.item(t).ownerNode.text;
					txt = txt.replace(/\/\*(\n|\r|.)*?\*\//g, ''); // strip comments
					// this pattern extracts all border-radius-containing rulesets
					// matches will be:
					// [0] = the whole lot
					// [1] = the selector text
					// [2] = all the rule text between braces
					// [3] = top/bottom and left/right parts if present (only if webkit/CSS3)
					// [4] = top|bottom
					// [5] = left|right
					// .. but 3..5 are useless as they're only the first match.
					var pat = new RegExp("^([\\w.#][\\w.#, ]+)[\\n\\s]*\\{([^}]+border-((top|bottom)-(left|right)-)?radius[^}]*)\\}", "mg");
					var matches;
					while ((matches = pat.exec(txt)) !== null) {
						var pat2 = new RegExp("(..)border-((top|bottom)-(left|right)-)?radius:\\s*([\\d.]+)(in|em|px|ex|pt)", "g");
						var submatches;
						while ((submatches = pat2.exec(matches[2])) !== null) {
						    if (submatches[1] !== "z-") {
						    	var tL,tR,bL,bR, tLu,tRu,bLu,bRu;					    							  
								if (!submatches[3]) { // no corner specified
									tL = tR = bL = bR = parseInt(submatches[5]);
									//tLu = tRu = bLu = bRu = submatches[6];
								}
								else { // corner specified
									propname = submatches[3].charAt(0) + submatches[4].charAt(0);
									this[propname + 'R'] = parseInt(submatches[5]);
									//this[propname + 'u'] = submatches[6];
								}
								var settings = {					
									tl: { radius: tL },
									tr: { radius: tR },
									bl: { radius: bL },
									br: { radius: bR }
								};	
								$(matches[1]).corner(settings);
						    }
						}
					}
				}
   			}
   		}
	});	
	
	$.fn.corner = function(options) {

		var settings = {
		  tl: { radius: 8 },
		  tr: { radius: 8 },
		  bl: { radius: 8 },
		  br: { radius: 8 },
		  antiAlias: true,
		  autoPad: true,
		  validTags: ["div"] 
		};
		if ( options && typeof(options) != 'string' )
			$.extend(settings, options);
	            
		return this.each(function() {
			if (!$(this).is('.hasCorners')) {
				applyCorners(this, settings);				
			}			
		}); 

  		// Apply the corners to the passed object!
		function applyCorners(box,settings)
		{
				
			// Setup Globals
			var $$ 						= $(box);
			this.topContainer 			= null;
			this.bottomContainer 		= null;
			this.shell            		= null;
			this.masterCorners 			= new Array();
			this.contentDIV 				= null;
		
			// Get CSS of box and define vars
			
			// Background + box colour
			this.x_bgi 				= $$.css("backgroundImage");																// Background Image
			this.x_bgi				= (x_bgi != "none" && x_bgi!="initial") ? x_bgi : "";
			this.x_bgr 				= $$.css("backgroundRepeat");																// Background repeat
			this.x_bgposX			= strip_px($$.css("backgroundPositionX")) ? strip_px($$.css("backgroundPositionX")) : 0; 	// Background position
			this.x_bgposY			= strip_px($$.css("backgroundPositionY")) ? strip_px($$.css("backgroundPositionY")) : 0; 	// Background position
			this.x_bgc 				= format_colour($$.css("backgroundColor"));													// Background Colour
			
			// Dimensions + positioning
			var x_height 			= $$.css("height");		 					// Height
			
			if(typeof x_height == 'undefined') x_height = 'auto';
			
			this.x_height       	= parseInt(((x_height != "" && x_height != "auto" && x_height.indexOf("%") == -1)? x_height.substring(0, x_height.indexOf("px")) : box.offsetHeight));
					
			if($.browser.msie && $.browser.version==6)
			{
				this.x_width	 		= strip_px(box.offsetWidth); 
			} 
			else 
			{
				this.x_width	 		= strip_px($$.css('width')); 
			}		

			this.xp_height      	= strip_px($$.parent().css("height")) ? strip_px($$.css("height")) : 'auto';				// Parent height
			
			// Borders
			this.x_bw		     	= strip_px($$.css("borderTopWidth")) ? strip_px($$.css("borderTopWidth")) : 0; 				// Border width
			
			// Different widths messed up borders
			this.x_bbw		     	= strip_px($$.css("borderBottomWidth")) ? strip_px($$.css("borderBottomWidth")) : 0; 		// Bottom Border width
			this.x_tbw		     	= strip_px($$.css("borderTopWidth")) ? strip_px($$.css("borderTopWidth")) : 0; 				// Top Border width
			this.x_lbw		     	= strip_px($$.css("borderLeftWidth")) ? strip_px($$.css("borderLeftWidth")) : 0; 			// Left Border width
			this.x_rbw		     	= strip_px($$.css("borderRightWidth")) ? strip_px($$.css("borderRightWidth")) : 0; 			// Right Border width
			
			/*this.x_bbw		     	= strip_px($$.css("borderTopWidth")) ? strip_px($$.css("borderTopWidth")) : 0;
			this.x_tbw		     	= strip_px($$.css("borderTopWidth")) ? strip_px($$.css("borderTopWidth")) : 0;
			this.x_lbw		     	= strip_px($$.css("borderTopWidth")) ? strip_px($$.css("borderTopWidth")) : 0;
			this.x_rbw		     	= strip_px($$.css("borderTopWidth")) ? strip_px($$.css("borderTopWidth")) : 0;*/
			
			
			this.x_bc		     	= format_colour($$.css("borderTopColor")); 													// Border colour
			this.x_bbc		     	= format_colour($$.css("borderBottomColor")); 												// Bottom Border colour
			this.x_tbc		     	= format_colour($$.css("borderTopColor")); 													// Top Border colour
			this.x_lbc		     	= format_colour($$.css("borderLeftColor")); 												// Left Border colour
			this.x_rbc		     	= format_colour($$.css("borderRightColor")); 												// Right Border colour
			this.borderString    	= this.x_bw + "px" + " solid " + this.x_bc;
      		this.borderStringB   	= this.x_bbw + "px" + " solid " + this.x_bbc;
      		this.borderStringR    	= this.x_rbw + "px" + " solid " + this.x_bc;
      		this.borderStringL   	= this.x_lbw + "px" + " solid " + this.x_bbc;
						
			// Padding
			this.x_pad		      	= strip_px($$.css("paddingTop"));															// Padding
			this.x_tpad	 			= strip_px($$.css("paddingTop"));															// Padding top
			this.x_bpad 			= strip_px($$.css("paddingBottom"));														// Padding Bottom
			this.x_lpad			 	= strip_px($$.css("paddingLeft"));															// Padding Left		
			this.x_rpad			 	= strip_px($$.css("paddingRight"));															// Padding Right
			this.topPaddingP     	= strip_px($$.parent().css("paddingTop"));													// Parent top padding
			this.bottomPaddingP  	= strip_px($$.parent().css("paddingBottom"));												// Parent bottom padding
			
			// Margins
			this.x_tmargin	 		= strip_px($$.css("marginTop"));														// Margin top
			this.x_bmargin 			= strip_px($$.css("marginBottom"));														// Margin Bottom
		
			// Calc Radius
			this.topMaxRadius = Math.max(settings.tl ? settings.tl.radius : 0, settings.tr ? settings.tr.radius : 0);
			this.botMaxRadius = Math.max(settings.bl ? settings.bl.radius : 0, settings.br ? settings.br.radius : 0);
			
			// Add styles and class		
			$$.addClass('hasCorners').css({
				"padding":				"0", 
				"border":				"none",
				"backgroundColor":		"transparent", 
				"backgroundImage":		"none", 
				'overflow':				"visible"
			});
		
			if(box.style.position != "absolute") $$.css("position","relative");

       		$$.attr("id","ccoriginaldiv");

			// Ok we add an inner div to actually put things into this will allow us to keep the height			
			var newMainContainer = document.createElement("div");
			
			$(newMainContainer).css({"padding" : "0", width: '100%' }).attr('id','ccshell');			

       		//this.shell = $$.append(newMainContainer);
       		this.shell = newMainContainer;
       		
      		//this.x_width = strip_px($(this.shell).css('width'));

			/*
			Create top and bottom containers.
			These will be used as a parent for the corners and bars.
			*/
			for(var t = 0; t < 2; t++)
			{
				switch(t)
				{
					// Top
					case 0:
					
						// Only build top bar if a top corner is to be draw
						if(settings.tl || settings.tr)
						{
							var newMainContainer = document.createElement("div");
							
							$(newMainContainer).css({
								width: '100%',
								"font-size":		"1px", 
								overflow:			"hidden", 
								position:			"absolute", 
								height:				topMaxRadius + "px",
								top:				0 - topMaxRadius + "px",
								"marginLeft" :			- parseInt( this.x_lbw + this.x_lpad) + "px",
								"marginRight" :			- parseInt( this.x_rbw + this.x_rpad) + "px"
							}).attr('id','cctopcontainer');
							
							if($.browser.msie && $.browser.version==6) {
								$(newMainContainer).css({   	
									"paddingLeft" :			Math.abs(parseInt( this.x_lbw + this.x_lpad)) + "px",
									"paddingRight" :		Math.abs(parseInt( this.x_rbw + this.x_rpad)) + "px"
						        });
					        }
          
							this.topContainer = this.shell.appendChild(newMainContainer);

						}
					break;
					
					// Bottom
					case 1:
					
						// Only build bottom bar if a bottom corner is to be draw
						if(settings.bl || settings.br)
						{							
							var newMainContainer = document.createElement("div");
							
							$(newMainContainer).css({ 
								width: '100%',
								"font-size":		"1px", 
								"overflow":			"hidden", 
								"position":			"absolute", 
								height:				botMaxRadius + "px",
								bottom:				0 - botMaxRadius  + "px",
								"marginLeft" :			- parseInt( this.x_lbw + this.x_lpad) + "px",
								"marginRight" :			- parseInt( this.x_rbw + this.x_rpad) + "px"
							}).attr('id','ccbottomcontainer');
							
							if($.browser.msie && $.browser.version==6) {
								$(newMainContainer).css({   	
									"paddingLeft" :			Math.abs(parseInt( this.x_lbw + this.x_lpad)) + "px",
									"paddingRight" :		Math.abs(parseInt( this.x_rbw + this.x_rpad)) + "px"
						        });
					        }
							
							this.bottomContainer = this.shell.appendChild(newMainContainer);	
						}
					break;
				}
			}


			// Create array of available corners
			var corners = ["tr", "tl", "br", "bl"];
			/*
			Loop for each corner
			*/
			for(var i in corners)
			{
				if(i > -1 < 4)
				{
					// Get current corner type from array
					var cc = corners[i];
					// Has the user requested the currentCorner be round?
					// Code to apply correct color to top or bottom
					if(cc == "tr" || cc == "tl")
					{
						var bwidth=this.x_bw;
						var bcolor=this.x_bc;
					} else {
						var bwidth=this.x_bbw;
						var bcolor=this.x_bbc;
					}
					
					// Yes, we need to create a new corner
					var newCorner = document.createElement("div");
					
					$(newCorner).css({
						position:"absolute",
						"font-size":"1px", 
						overflow:"hidden"
					}).height(settings[cc].radius + "px").width(settings[cc].radius + "px");
					
					// THE FOLLOWING BLOCK OF CODE CREATES A ROUNDED CORNER
					// ---------------------------------------------------- TOP
					// Get border radius
					var borderRadius = parseInt(settings[cc].radius - bwidth);
					// Cycle the x-axis
					for(var intx = 0, j = settings[cc].radius; intx < j; intx++)
					{
                      // Calculate the value of y1 which identifies the pixels inside the border
                      if((intx +1) >= borderRadius)
                        var y1 = -1;
                      else
                        var y1 = (Math.floor(Math.sqrt(Math.pow(borderRadius, 2) - Math.pow((intx+1), 2))) - 1);
                      // Only calculate y2 and y3 if there is a border defined
                      if(borderRadius != j)
                      {
                          if((intx) >= borderRadius)
                            var y2 = -1;
                          else
                            var y2 = Math.ceil(Math.sqrt(Math.pow(borderRadius,2) - Math.pow(intx, 2)));
                           if((intx+1) >= j)
                            var y3 = -1;
                           else
                            var y3 = (Math.floor(Math.sqrt(Math.pow(j ,2) - Math.pow((intx+1), 2))) - 1);
                      }
                      // Calculate y4
                      if((intx) >= j)
                        var y4 = -1;
                      else
                        var y4 = Math.ceil(Math.sqrt(Math.pow(j ,2) - Math.pow(intx, 2)));
                      // Draw bar on inside of the border with foreground colour
                      
                      if(y1 > -1) drawPixel(intx, 0, this.x_bgc, 100, (y1+1), newCorner, -1, settings[cc].radius, 0, this.x_bgi, this.x_width, this.x_height, this.x_bw, this.x_bgr);
                      // Only draw border/foreground antialiased pixels and border if there is a border defined
                      if(borderRadius != j)
                      {
                          // Cycle the y-axis
                          for(var inty = (y1 + 1); inty < y2; inty++)
                          {
                              // Draw anti-alias pixels
                              if(settings.antiAlias)
                              {
                                  // For each of the pixels that need anti aliasing between the foreground and border colour draw single pixel divs
                                  if(this.x_bgi != "")
                                  {
										var borderFract = (pixelFraction(intx, inty, borderRadius) * 100);
										if(borderFract < 30)
										{
											drawPixel(intx, inty, bcolor, 100, 1, newCorner, 0, settings[cc].radius, 0, this.x_bgi, this.x_width, this.x_height, bwidth, this.x_bgr);
										}
										else
										{
											drawPixel(intx, inty, bcolor, 100, 1, newCorner, -1, settings[cc].radius, 0, this.x_bgi, this.x_width, this.x_height, bwidth, this.x_bgr);
                                  		}
                                  	}
                                  	else
                                  	{
                                      	var pixelcolour = BlendColour(this.x_bgc, bcolor, pixelFraction(intx, inty, borderRadius));
                                     	drawPixel(intx, inty, pixelcolour, 100, 1, newCorner, 0, settings[cc].radius, 0, this.x_bgi, this.x_width, this.x_height, bwidth, this.x_bgr);
                                  	}
                              }
                          }
                          // Draw bar for the border
                          if(settings.antiAlias)
                          {
                              if(y3 >= y2)
                              {
                                 if (y2 == -1) y2 = 0;
                                 drawPixel(intx, y2, bcolor, 100, (y3 - y2 + 1), newCorner, 0, 0, 1, this.x_bgi, this.x_width, this.x_height, bwidth, this.x_bgr);
                              }
                          }
                          else
                          {
                              if(y3 >= y1)
                              {
                                  drawPixel(intx, (y1 + 1), bcolor, 100, (y3 - y1), newCorner, 0, 0, 1, this.x_bgi, this.x_width, this.x_height, bwidth, this.x_bgr);
                              }
                          }
                          // Set the colour for the outside curve
                          var outsideColour = bcolor;
                      }
                      else
                      {
                          // Set the colour for the outside curve
                          var outsideColour = this.x_bgc;
                          var y3 = y1;
                      }
                      // Draw aa pixels?
                      if(settings.antiAlias)
                      {
                          // Cycle the y-axis and draw the anti aliased pixels on the outside of the curve
                          for(var inty = (y3 + 1); inty < y4; inty++)
                          {
                              // For each of the pixels that need anti aliasing between the foreground/border colour & background draw single pixel divs
                              drawPixel(intx, inty, outsideColour, (pixelFraction(intx, inty , j) * 100), 1, newCorner, ((bwidth > 0)? 0 : -1), settings[cc].radius, 0, this.x_bgi, this.x_width, this.x_height, bwidth);
                          }
                      }
                  }
                  // END OF CORNER CREATION
                  // ---------------------------------------------------- END
                  
                  // We now need to store the current corner in the masterConers array
                  masterCorners[settings[cc].radius] = $(newCorner).clone();

                  /*
                  Now we have a new corner we need to reposition all the pixels unless
                  the current corner is the bottom right.
                  */
                  // Loop through all children (pixel bars)
                  for(var t = 0, k = newCorner.childNodes.length; t < k; t++)
                  {
                      // Get current pixel bar
                      var pixelBar = newCorner.childNodes[t];
                      
                      // Get current top and left properties
                      var pixelBarTop    = parseInt(pixelBar.style.top.substring(0, pixelBar.style.top.indexOf("px")));
                      var pixelBarLeft   = parseInt(pixelBar.style.left.substring(0, pixelBar.style.left.indexOf("px")));
                      var pixelBarHeight = parseInt(pixelBar.style.height.substring(0, pixelBar.style.height.indexOf("px")));
                     
                      // Reposition pixels
                      if(cc == "tl" || cc == "bl"){
                          pixelBar.style.left = settings[cc].radius -pixelBarLeft -1 + "px"; // Left
                      }
                      if(cc == "tr" || cc == "tl"){
                          pixelBar.style.top =  settings[cc].radius -pixelBarHeight -pixelBarTop + "px"; // Top
                      }
                      pixelBar.style.backgroundRepeat = this.x_bgr;

                      switch(cc)
                      {
                          case "tr":
 								
 								if($.browser.msie && $.browser.version==6) var offset = this.x_lpad + this.x_rpad + this.x_lbw + this.x_rbw;
 								else var offset = 0;
 								
                                pixelBar.style.backgroundPosition  = parseInt( this.x_bgposX - Math.abs( this.x_rbw - this.x_lbw + (this.x_width - settings[cc].radius + this.x_rbw) + pixelBarLeft) - settings.bl.radius - this.x_bw - settings.br.radius - this.x_bw) + offset + "px " + parseInt( this.x_bgposY - Math.abs(settings[cc].radius -pixelBarHeight -pixelBarTop - this.x_bw)) + "px";

                              break;
                          case "tl":
                              pixelBar.style.backgroundPosition = parseInt( this.x_bgposX - Math.abs((settings[cc].radius -pixelBarLeft -1)  - this.x_lbw)) + "px " + parseInt( this.x_bgposY - Math.abs(settings[cc].radius -pixelBarHeight -pixelBarTop - this.x_bw)) + "px";
                              break;
                          case "bl":

                                  pixelBar.style.backgroundPosition = parseInt( this.x_bgposX - Math.abs((settings[cc].radius -pixelBarLeft -1) - this.x_lbw )) + "px " + parseInt( this.x_bgposY - Math.abs(( this.x_height + (this.x_bw+this.x_tpad+this.x_bpad) - settings[cc].radius + pixelBarTop))) + "px";
              
                              break;
                          case "br":
								  // Added - settings.bl.radius - this.x_bw - settings.br.radius - this.x_bw to this and tr to offset background image due to neg margins.
								  if($.browser.msie && $.browser.version==6) var offset = this.x_lpad + this.x_rpad + this.x_lbw + this.x_rbw;
 									else var offset = 0;
 								
                                  pixelBar.style.backgroundPosition = parseInt( this.x_bgposX - Math.abs( this.x_rbw - this.x_lbw + (this.x_width - settings[cc].radius + this.x_rbw) + pixelBarLeft) - settings.bl.radius - this.x_bw - settings.br.radius - this.x_bw) + offset + "px " + parseInt( this.x_bgposY - Math.abs(( this.x_height + (this.x_bw+this.x_tpad+this.x_bpad) - settings[cc].radius + pixelBarTop))) + "px";
                      
                              break;
                      }
                  }

                  // Position the container
                  switch(cc)
                  {


                      case "tl":
                        if(newCorner.style.position == "absolute") newCorner.style.top  = "0px";
                        if(newCorner.style.position == "absolute") newCorner.style.left = "0px";
                        if(this.topContainer) temp= this.topContainer.appendChild(newCorner);
						$(temp).attr("id","cctl");


                        break;
                      case "tr":
                        if(newCorner.style.position == "absolute") newCorner.style.top  = "0px";
                        if(newCorner.style.position == "absolute") newCorner.style.right = "0px";
                        if(this.topContainer) temp= this.topContainer.appendChild(newCorner);
						$(temp).attr("id","cctr");

                        break;
                      case "bl":
                        if(newCorner.style.position == "absolute") newCorner.style.bottom  = "0px";
                        if(newCorner.style.position == "absolute") newCorner.style.left = "0px";
						if(this.bottomContainer) temp= this.bottomContainer.appendChild(newCorner);
						$(temp).attr("id","ccbl");

                        break;
                      case "br":
                        if(newCorner.style.position == "absolute") newCorner.style.bottom   = "0px";
                        if(newCorner.style.position == "absolute") newCorner.style.right = "0px";
						if(this.bottomContainer) temp= this.bottomContainer.appendChild(newCorner);
						$(temp).attr("id","ccbr");

                        break;
                  }

              }
          }



          /*
          The last thing to do is draw the rest of the filler DIVs.
          We only need to create a filler DIVs when two corners have
          diffrent radiuses in either the top or bottom container.
          */

          // Find out which corner has the bigger radius and get the difference amount
          var radiusDiff = new Array();
          radiusDiff["t"] = Math.abs(settings.tl.radius - settings.tr.radius);
          radiusDiff["b"] = Math.abs(settings.bl.radius - settings.br.radius);

          for(z in radiusDiff)
          {
              // FIX for prototype lib
              if(z == "t" || z == "b")
              {
                  if(radiusDiff[z])
                  {
                      // Get the type of corner that is the smaller one
                      var smallerCornerType = ((settings[z + "l"].radius < settings[z + "r"].radius)? z +"l" : z +"r");

                      // First we need to create a DIV for the space under the smaller corner
                      var newFiller = document.createElement("DIV");
                      newFiller.style.height = radiusDiff[z] + "px";
                      newFiller.style.width  =  settings[smallerCornerType].radius + "px";
                      newFiller.style.position = "absolute";
                      newFiller.style.fontSize = "1px";
                      newFiller.style.overflow = "hidden";
                      newFiller.style.backgroundColor = this.x_bgc;
                      //newFiller.style.backgroundColor = get_random_color();

                      // Position filler
                      switch(smallerCornerType)
                      {
                          case "tl":
                              newFiller.style.bottom = "0px";
                              newFiller.style.left   = "0px";
                              newFiller.style.borderLeft = this.borderString;
                              temp=this.topContainer.appendChild(newFiller);
temp.id="cctlfiller";

                              break;

                          case "tr":
                              newFiller.style.bottom = "0px";
                              newFiller.style.right  = "0px";
                              newFiller.style.borderRight = this.borderString;
                              temp=this.topContainer.appendChild(newFiller);
temp.id="cctrfiller";

                              break;

                          case "bl":
                              newFiller.style.top    = "0px";
                              newFiller.style.left   = "0px";
                              newFiller.style.borderLeft = this.borderStringB;
                              temp=this.bottomContainer.appendChild(newFiller);
temp.id="ccblfiller";

                              break;

                          case "br":
                              newFiller.style.top    = "0px";
                              newFiller.style.right  = "0px";
                              newFiller.style.borderRight = this.borderStringB;
                              temp=this.bottomContainer.appendChild(newFiller);
temp.id="ccbrfiller";

                              break;
                      }
                  }














                  // Create the bar to fill the gap between each corner horizontally
                  var newFillerBar = document.createElement("div");
                  newFillerBar.style.position = "relative";
                  newFillerBar.style.fontSize = "1px";
                  newFillerBar.style.overflow = "hidden";
                  newFillerBar.style.backgroundColor = this.x_bgc;
                  newFillerBar.style.backgroundImage = this.x_bgi;
                  newFillerBar.style.backgroundRepeat= this.x_bgr;


                  switch(z)
                  {
                      case "t":
                          // Top Bar
                          if(this.topContainer)
                          {
                              // Edit by Asger Hallas: Check if settings.xx.radius is not false
                              if(settings.tl.radius && settings.tr.radius)
                              {
                                  newFillerBar.style.height      = 100 + topMaxRadius - this.x_tbw + "px";
                                  newFillerBar.style.marginLeft  = settings.tl.radius - this.x_lbw + this.x_rbw + "px";
                                  newFillerBar.style.marginRight = settings.tr.radius - this.x_lbw + this.x_rbw + "px";
                                  newFillerBar.style.borderTop   = this.borderString;                                 
                                   
                                  if(this.x_bgi != "")
                                    newFillerBar.style.backgroundPosition  = parseInt( this.x_bgposX - (topMaxRadius - this.x_lbw )) + "px " + parseInt( this.x_bgposY ) + "px";
                                    //newFillerBar.style.backgroundPosition  = parseInt( this.x_bgposX - (topMaxRadius - this.x_lbw)) + "px " + parseInt( this.x_bgposY ) + "px";
                                    
									if($.browser.msie && $.browser.version==6) {
										$(newFillerBar).css({   	
											"marginLeft" :		- parseInt( this.x_lbw + this.x_lpad - settings.tl.radius ) + "px",
											"marginRight" :		- parseInt( this.x_rbw + this.x_rpad - settings.tr.radius ) + "px"
										});
										 if(this.x_bgi != "")
                                    		newFillerBar.style.backgroundPosition  = parseInt( this.x_bgposX + this.x_lbw - (topMaxRadius )) + "px " + parseInt( this.x_bgposY ) + "px";
					       			}

								
                                  temp=this.topContainer.appendChild(newFillerBar);
								  $(temp).attr("id","cctopmiddlefiller");

                                  // Repos the boxes background image
                                  $(this.shell).css("backgroundPosition", parseInt( this.x_bgposX ) + "px " + parseInt( this.x_bgposY - (topMaxRadius - this.x_lbw)) + "px");
                              }
                          }
                          break;
                      case "b":
                          if(this.bottomContainer)
                          {
                              // Edit by Asger Hallas: Check if settings.xx.radius is not false
                              if(settings.bl.radius && settings.br.radius)
                              {
                                  // Bottom Bar
                                  newFillerBar.style.height     = botMaxRadius - this.x_bbw + "px";
                                  newFillerBar.style.marginLeft   = settings.bl.radius - this.x_lbw + this.x_rbw + "px";
                                  newFillerBar.style.marginRight  = settings.br.radius - this.x_lbw + this.x_rbw + "px";
                                  newFillerBar.style.borderBottom = this.borderStringB;
                                  if(this.x_bgi != "")
                                    newFillerBar.style.backgroundPosition  = parseInt( this.x_bgposX - (botMaxRadius - this.x_lbw )) + "px " + parseInt( this.x_bgposY - (this.x_height + this.x_tpad + this.x_bbw + this.x_bpad - botMaxRadius )) + "px";
                                    //newFillerBar.style.backgroundPosition  = parseInt( this.x_bgposX - (botMaxRadius  - this.x_lbw )) + "px " + parseInt( this.x_bgposY - (this.x_height + this.x_tpad + this.x_bw + this.x_bpad - botMaxRadius )) + "px";
                                  
                                  if($.browser.msie && $.browser.version==6) {
										$(newFillerBar).css({   	
											"marginLeft" :		- parseInt( this.x_lbw + this.x_lpad - settings.bl.radius ) + "px",
											"marginRight" :		- parseInt( this.x_rbw + this.x_rpad - settings.br.radius ) + "px"
										});
										
										if(this.x_bgi != "")
                                    		newFillerBar.style.backgroundPosition  = parseInt( this.x_bgposX - (botMaxRadius - this.x_lbw)) + "px " + parseInt( this.x_bgposY - (this.x_height + this.x_tpad + this.x_bbw + this.x_bpad - botMaxRadius )) + "px";
					       			}
					       			
                                  temp=this.bottomContainer.appendChild(newFillerBar);
$(temp).attr("id","ccbottommiddlefiller");

                              }
                          }
                          break;
                  }
              }
          }


          // Create content container
          var contentContainer = document.createElement("div");
          var pd = 0;
          // Set contentContainer's properties
        //  contentContainer.style.position = "absolute";
          contentContainer.className      = "autoPadDiv";
          // Get padding amounts
          var topPadding = Math.abs( this.x_bw  + this.x_pad);
          var botPadding = Math.abs( this.x_bbw + this.x_bpad);
          
          // Apply top padding
          if(topMaxRadius < this.boxPadding)
            {
             contentContainer.style.paddingTop = Math.abs(parseInt( pd + topPadding)) + "px";
            } 
            else
            {
            contentContainer.style.paddingTop = "0";
          }
          
          // Apply Bottom padding
          if(botMaxRadius < this.x_pad)
            {contentContainer.style.paddingBottom = Math.abs(parseInt(botPadding - botMaxRadius)) + "px";} else
            {contentContainer.style.paddingBottom = "0";}
          
          // Content container must fill vertically to show the border
          $(contentContainer).css({   	
			"marginLeft" :			- parseInt( this.x_lbw + this.x_lpad) + "px",
			"marginRight" :			- parseInt( this.x_rbw + this.x_rpad) + "px",			
			"marginTop" :			"-" + Math.abs(parseInt( this.x_tbw + (this.x_tpad - topMaxRadius))) + "px",
			"marginBottom" :		"-" + Math.abs(parseInt( this.x_bbw + (this.x_bpad - botMaxRadius))) + "px",
			"border-left" :				this.borderStringL,
			"border-right" :			this.borderStringR,
			"border-top" :				this.borderString,
			"border-bottom" :			this.borderStringB,
			"borderTopWidth" :		"0",
			"borderBottomWidth" :	"0",
			"height" : 				"100%",
			"width" : "100%",
			"paddingLeft" :			Math.abs(parseInt( this.x_lpad)) + "px",
			"paddingRight" :		Math.abs(parseInt( this.x_rpad)) + "px",
			"paddingTop" :			Math.abs(parseInt( this.x_tbw + (this.x_tpad - topMaxRadius))) + "px",
			"paddingBottom" :   	Math.abs(parseInt( this.x_bbw + (this.x_bpad - botMaxRadius))) + "px"
          });
          
          // Origional container has background image
          $$.css({            
          	"paddingLeft" :			Math.abs(parseInt( this.x_lbw + this.x_lpad)) + "px",
			"paddingRight" :		Math.abs(parseInt( this.x_rbw + this.x_rpad)) + "px",
			"paddingTop" :			Math.abs(parseInt( this.x_tbw + (this.x_tpad - topMaxRadius))) + "px",
			"paddingBottom" :      	Math.abs(parseInt( this.x_bbw + (this.x_bpad - botMaxRadius))) + "px",
			"backgroundColor" : 	this.x_bgc,
			"backgroundImage" :		this.x_bgi,
			"backgroundPosition" : 	this.x_bw + 'px -' + Math.abs(parseInt(topMaxRadius - this.x_bw )) + "px",
			'margin-top':			0, 
		  	'margin-bottom':		0
          });
          
          //'margin-top':			parseInt(this.x_tmargin + topMaxRadius) + "px", 
		  //'margin-bottom':		parseInt(this.x_bmargin + botMaxRadius) + "px"
         
          // IE does not like an empty box; without this it won't show the contentContainer
          if ($$.html() == "") $$.html('&nbsp;');
                    
          // Append contentContainer
          $$.wrapInner(contentContainer);          
          $$.prepend(this.shell);
          
          // Wrapper to make margins work correctly
          var wrapper = document.createElement("div");
          
          $(wrapper).css({            
			'margin-top':			parseInt(this.x_tmargin) + "px", 
			'margin-bottom':		parseInt(this.x_bmargin) + "px",
			'padding-top':			topMaxRadius + "px", 
			'padding-bottom':		botMaxRadius + "px",
			'overflow': 'hidden'
          }).addClass('ccwrapper');
          
          $$.wrap(wrapper);
                   
          // Because of this method of doing the corners we have magins above and below; the following prevents margin collapsing.
          $$.after('<div class="clear" style="height:0;line-height:0px;">&nbsp;</div>');
      }		
		
		/*
		This function draws the pixels
		*/	
		function drawPixel( intx, inty, colour, transAmount, height, newCorner, image, cornerRadius, isBorder, bgImage, x_width, x_height, x_bw, repeat ) {
			
			//var $$ = $(box);			
			
		    var pixel = document.createElement("div");
		    
		    $(pixel).css({	
		    	"height" :			height, 
		    	"width" :			"1px", 
		    	"position" :		"absolute", 
		    	"font-size" :		"1px", 
		    	"overflow" :		"hidden",
		    	"top" :				inty + "px",
		    	"left" :			intx + "px",
		    	"background-color" :colour
		    });
		    
		    // Max Top Radius
		    var topMaxRadius = Math.max(settings.tl ? settings.tl.radius : 0, settings.tr ? settings.tr.radius : 0);
		    
		    // Dont apply background image to border pixels
			if(image == -1 && bgImage !="")
			{
				$(pixel).css({
					"background-position":"-" + Math.abs(x_width - (cornerRadius - intx) + x_bw) + "px -" + Math.abs((x_height + topMaxRadius + inty) -x_bw) + "px",
					"background-image":bgImage,
					"background-repeat":repeat					 
				});
			}
			else
			{
				if (!isBorder) $(pixel).addClass('hasBackgroundColor');
			}		    
		    if (transAmount != 100)
		    	$(pixel).css({opacity: (transAmount/100) });

		    newCorner.appendChild(pixel);
		};		
				
		
		// Utilities
		function BlendColour(Col1, Col2, Col1Fraction) 
		{
			
			var red1 = parseInt(Col1.substr(1,2),16);
			var green1 = parseInt(Col1.substr(3,2),16);
			var blue1 = parseInt(Col1.substr(5,2),16);
			var red2 = parseInt(Col2.substr(1,2),16);
			var green2 = parseInt(Col2.substr(3,2),16);
			var blue2 = parseInt(Col2.substr(5,2),16);
			
			if(Col1Fraction > 1 || Col1Fraction < 0) Col1Fraction = 1;
			
			var endRed = Math.round((red1 * Col1Fraction) + (red2 * (1 - Col1Fraction)));
			if(endRed > 255) endRed = 255;
			if(endRed < 0) endRed = 0;
			
			var endGreen = Math.round((green1 * Col1Fraction) + (green2 * (1 - Col1Fraction)));
			if(endGreen > 255) endGreen = 255;
			if(endGreen < 0) endGreen = 0;
			
			var endBlue = Math.round((blue1 * Col1Fraction) + (blue2 * (1 - Col1Fraction)));
			if(endBlue > 255) endBlue = 255;
			if(endBlue < 0) endBlue = 0;
			
			return "#" + IntToHex(endRed)+ IntToHex(endGreen)+ IntToHex(endBlue);
			
		}
	
		function IntToHex(strNum) 
		{			
			rem = strNum % 16;
			base = Math.floor(strNum / 16);
			
			baseS = MakeHex(base);
			remS = MakeHex(rem);
			
			return baseS + '' + remS;
		}
	
		function MakeHex(x)
		{
		  if((x >= 0) && (x <= 9))
		  {
		      return x;
		  }
		  else
		  {
		      switch(x)
		      {
		          case 10: return "A";
		          case 11: return "B";
		          case 12: return "C";
		          case 13: return "D";
		          case 14: return "E";
		          case 15: return "F";
		      }
		  }
		}
	
		/*
		For a pixel cut by the line determines the fraction of the pixel on the 'inside' of the
		line.  Returns a number between 0 and 1
		*/
		function pixelFraction(x, y, r)
		{
			var pixelfraction = 0;
			
			/*
			determine the co-ordinates of the two points on the perimeter of the pixel that the
			circle crosses
			*/
			var xvalues = new Array(1);
			var yvalues = new Array(1);
			var point = 0;
			var whatsides = "";
			
			// x + 0 = Left
			var intersect = Math.sqrt((Math.pow(r,2) - Math.pow(x,2)));
			
			if ((intersect >= y) && (intersect < (y+1)))
			{
				whatsides = "Left";
				xvalues[point] = 0;
				yvalues[point] = intersect - y;
				point =  point + 1;
			}
			// y + 1 = Top
			var intersect = Math.sqrt((Math.pow(r,2) - Math.pow(y+1,2)));
			
			if ((intersect >= x) && (intersect < (x+1)))
			{
				whatsides = whatsides + "Top";
				xvalues[point] = intersect - x;
				yvalues[point] = 1;
				point = point + 1;
			}
			// x + 1 = Right
			var intersect = Math.sqrt((Math.pow(r,2) - Math.pow(x+1,2)));
			
			if ((intersect >= y) && (intersect < (y+1)))
			{
				whatsides = whatsides + "Right";
				xvalues[point] = 1;
				yvalues[point] = intersect - y;
				point =  point + 1;
			}
			// y + 0 = Bottom
			var intersect = Math.sqrt((Math.pow(r,2) - Math.pow(y,2)));
			
			if ((intersect >= x) && (intersect < (x+1)))
			{
				whatsides = whatsides + "Bottom";
				xvalues[point] = intersect - x;
				yvalues[point] = 0;
			}
			
			/*
			depending on which sides of the perimeter of the pixel the circle crosses calculate the
			fraction of the pixel inside the circle
			*/
			switch (whatsides)
			{
			      case "LeftRight":
			      pixelfraction = Math.min(yvalues[0],yvalues[1]) + ((Math.max(yvalues[0],yvalues[1]) - Math.min(yvalues[0],yvalues[1]))/2);
			      break;
			
			      case "TopRight":
			      pixelfraction = 1-(((1-xvalues[0])*(1-yvalues[1]))/2);
			      break;
			
			      case "TopBottom":
			      pixelfraction = Math.min(xvalues[0],xvalues[1]) + ((Math.max(xvalues[0],xvalues[1]) - Math.min(xvalues[0],xvalues[1]))/2);
			      break;
			
			      case "LeftBottom":
			      pixelfraction = (yvalues[0]*xvalues[1])/2;
			      break;
			
			      default:
			      pixelfraction = 1;
			}
			
			return pixelfraction;
		}
  
  
		// This function converts CSS rgb(x, x, x) to hexadecimal
		function rgb2Hex(rgbColour)
		{
			try{
			
				// Get array of RGB values
				var rgbArray = rgb2Array(rgbColour);
				
				// Get RGB values
				var red   = parseInt(rgbArray[0]);
				var green = parseInt(rgbArray[1]);
				var blue  = parseInt(rgbArray[2]);
				
				// Build hex colour code
				var hexColour = "#" + IntToHex(red) + IntToHex(green) + IntToHex(blue);
			}
			catch(e){			
				alert("There was an error converting the RGB value to Hexadecimal in function rgb2Hex");
			}
			
			return hexColour;
		}
		
		// Returns an array of rbg values
		function rgb2Array(rgbColour)
		{
			// Remove rgb()
			var rgbValues = rgbColour.substring(4, rgbColour.indexOf(")"));
			
			// Split RGB into array
			var rgbArray = rgbValues.split(", ");
			
			return rgbArray;
		}	

		// Formats colours
		function format_colour(colour)
		{
			var returnColour = "#ffffff";
			
			// Make sure colour is set and not transparent
			if(colour != "" && colour != "transparent")
			{
				// RGB Value?
				if(colour.substr(0, 3) == "rgb" && colour.substr(0, 4) != "rgba")
				{
				  // Get HEX aquiv.
				  returnColour = rgb2Hex(colour);
				}
				else if(colour.length == 4)
				{
				  // 3 chr colour code add remainder
				  returnColour = "#" + colour.substring(1, 2) + colour.substring(1, 2) + colour.substring(2, 3) + colour.substring(2, 3) + colour.substring(3, 4) + colour.substring(3, 4);
				}
				else
				{
				  // Normal valid hex colour
				  returnColour = colour;
				}
			}
			
			return returnColour;
		}
		
		// Removes 'px' from string
		function strip_px(value) 
		{
			if (typeof(value)!='string') return value;
			return parseInt((( value != "auto" && value.indexOf("%") == -1 && value != "" && value.indexOf("px") !== -1)? Math.round(value.slice(0, value.indexOf("px"))) : 0))
		}
			
	};
})(jQuery);

