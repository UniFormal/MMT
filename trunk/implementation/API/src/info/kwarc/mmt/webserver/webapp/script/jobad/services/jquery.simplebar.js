/**
 * jquery.simplebar - a simple bottom bar plugin
 *
 * Copyright (c) 2010 Catalin David
 * http://kwarc.info
 *
 * Licensed under Public Domain
 *
 */
(function($){

    function SimpleBar(elem, conf){
        var self = this;
        elem = jQuery(elem);
        
        var bar = jQuery(document.createElement('div')).attr('style', "overflow: hidden;width: 100%;height: 38px;position: fixed;bottom: 0;left: 0;color: #000;font-family: Arial;font-weight: bold;background-image: url('./tui.png');").addClass('floatingbar').appendTo(elem);
        var ul = jQuery(document.createElement('ul')).attr('id', 'simplebarULid').appendTo(bar);
        
        jQuery.extend(self, {
            getVersion: function(){
                return [0, 0, 1];
            },
            getParent: function(){
                return elem;
            },
            getBar: function(){
                return bar;
            },
            addItem: function(id, content){
                var newli = jQuery(document.createElement('li')).attr('id', id).html(content).appendTo(ul);
                return self;
            },
            removeItem: function(id){
                ul.children('#' + id).remove();
                return self;
            },
			editItem: function(id, content){
				if(ul.children('#' + id).length > 0)//if it exists
					ul.children('#' + id).html(content);
				else{
					var newli = jQuery(document.createElement('li')).attr('id', id).html(content).appendTo(ul);
				}
				return self;
			},
            show: function(){
                conf.onBeforeShow.call(self);
                
                //effects?
                switch (conf.showEffect) {
                    case 'fade':
                        bar.fadeIn(conf.showTime);
                        break;
                    case 'slide':
                        bar.slideDown(conf.showTime);
                        break;
                    case 'custom':
                        conf.showCustom.call(bar, conf.showTime);
                        break;
                    default:
                    case 'none':
                        bar.css({
                            height: 0
                        }).animate({
                            height: '38'
                        }, 'slow');
                        break;
                }
                
                conf.onShow.call(self);
                return self;
            },
            hide: function(){
                conf.onBeforeHide.call(self);
                switch (conf.hideEffects) {
                    case 'fade':
                        bar.fadeOut(conf.hideTime);
                        break;
                    case 'slide':
                        bar.slideUp(conf.hideTime);
                        break;
                    case 'custom':
                        conf.hideCustom.call(bar, conf.hideTime);
                        break;
                    default:
                    case 'none':
                        bar.hide();
                        break;
                }
                conf.onHide.call(self);
                return self;
            },
            update: function(content){
                bar.html(content);
                conf.content = content;
                
                return self;
            }
        })
        if (!conf.hidden) {
            self.show();
        }
        else {
            self.hide();
        }
        
    }
    
    jQuery.fn.simplebar = function(conf){
        var api = jQuery(this).eq(typeof conf == 'number' ? conf : 0).data("simplebar");
        if (api) 
            return api;
        
        var defaultConf = {
            hidden: false,
            
            showEffect: 'fade',
            showTime: 200,
            showCustom: null,
            hideEffect: 'fade',
            hideTime: 150,
            hideCustom: null,
            
            onBeforeShow: function(){
            },
            onShow: function(){
            },
            onBeforeHide: function(){
            },
            onHide: function(){
            }
        };
        
        jQuery.extend(defaultConf, conf);
        this.each(function(){
            var el = new SimpleBar(jQuery(this), defaultConf);
            jQuery(this).data("simplebar", el);
        })
        
        return this;
    }
})();
