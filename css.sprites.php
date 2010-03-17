<?php
/**
 * File from WEBO Site SpeedUp, WEBO Software (http://www.webogroup.com/)
 * Parses content, gets all CSS images, converts them to CSS Sprites.
 * Outputs minified (no sorting selectors) content
 *
 **/
class css_sprites {

	/**
	* Constructor
	* Sets the options
	**/
	function css_sprites ($css_code = '', $options = array()) {
/* create global object for optimization logic */
		$this->optimizer = new css_sprites_optimize($options);
		if (!empty($css_code)) {
/* convert CSS code to hash */
			$this->optimizer->css = new csstidy();
			$this->optimizer->css->load_template($this->optimizer->root_dir . 'libs/php/css.template.tpl');
			$this->optimizer->css->parse($css_code);
		}
	}
	/**
	* Main function
	* Process with given data
	**/
	function process() {
		
		if (empty($this->optimizer->no_sprites) && !empty($this->optimizer->css)) {
			foreach ($this->optimizer->css->css as $import => $token) {
/* create array for selectors with background images */
				$this->optimizer->media[$import] = array();
				foreach ($token as $tags => $rule) {
					foreach ($rule as $key => $value) {
/* standartize all background values from input */
						if (strpos(" ". $key, "background")) {
/* rewrite current background with strict none */
							if ($key == 'background' && ($value == 'none !important' || $value == 'none')) {
								$this->optimizer->css->css[$import][$tags]['background'] = $this->optimizer->none;
							}
							foreach (explode(",", $tags) as $tag) {
/* create new item (array) if required */
								if (empty($this->optimizer->media[$import][$tag])) {
									$this->optimizer->media[$import][$tag] = array();
								}

								if ($key == 'background') {
/* resolve background property */
									if ($value == 'none') {
										$background = array('background-image' => $this->optimizer->none);
									} else {
										$background = $this->optimizer->css->optimise->dissolve_short_bg($value);
									}
									foreach ($background as $bg => $property) {
/* skip default properties */
										if (!($bg == 'background-position' &&
												($property == '0 0 !important' ||
												$property == 'top left !important' ||
												$property == '0 0' ||
												$property == 'top left')) &&
											!($bg == 'background-origin' &&
												($property == 'padding !important' ||
												$property == 'padding')) &&
											!($bg == 'background-color' &&
												($property == 'transparent !important' ||
												$property == 'transparent')) &&
											!($bg == 'background-clip' &&
												($property == 'border !important' ||
												$property == 'border')) &&
											!($bg == 'background-attachement' &&
												($property == 'scroll !important' ||
												$property =='scroll')) &&
											!($bg == 'background-size' &&
												($property == 'auto !important' ||
												$property == 'auto')) &&
											!($bg == 'background-repeat' &&
												($property == 'repeat !important' ||
												$property == 'repeat'))) {
													$property = $this->normalize_property($property);
													if ($bg == 'background-image' && ($property == 'none !important' || $property == 'none')) {
														$property = $this->optimizer->none;
													}
/* compute background-position to standard view */
													if ($bg == 'background-position') {
														$property = $this->compute_background_position($property);
													}
													$this->optimizer->media[$import][$tag][$bg] = $property;
										}
									}
								} else {
									$value = $this->normalize_property($value);
/* fix background-position: top|bottom -> center top|center bottom */
									if ($key == 'background-position') {
										$value = $this->compute_background_position($value);
									}
/* skip default properties */
									if ($key != 'background-position' || $value != '0 0') {
										$this->optimizer->media[$import][$tag][$key] = $value;
									}
								}
/* remember multiple selectors to unset them */
								$this->optimizer->media[$import][$tag]['tags'] = $tags;
							}
						}
					}
				}
			}
/* fill images arrays with possible dimensions */
			foreach ($this->optimizer->css->css as $import => $token) {
				foreach ($token as $tags => $rule) {
					foreach ($rule as $property => $value) {
						if ($property == 'width' || $property == 'height' || strpos(" " .$property, 'padding')) {
/* try to add all possible dimensial properties for selected tags with background */
							foreach ($this->optimizer->media as $imp => $images) {
								foreach ($images as $key => $image) {
									$fixed_key = $this->fix_css3_selectors($key);
/* remove pseudo-selectors, i.e. :focus, :hover, etc*/
									if (in_array($key, explode(",", $tags)) || in_array($fixed_key, explode(",", $tags))) {
										if (strpos(" " .$property, 'padding')) {
											if ($property == 'padding') {
												$padding = $this->optimizer->css->optimise->dissolve_4value_shorthands($property, $value);
											} else {
												$padding = array(
													$property => $value
												);
											}
											foreach ($padding as $prop => $val) {
												$this->optimizer->media[$imp][$key][$prop] = $val;
											}
										} else {
											$this->optimizer->media[$imp][$key][$property] = $value;
										}
									}
								}
							}
						}
					}
				}
			}
			if ($this->optimizer->restore_properties) {
				$properties = array(
					'background-image',	'background-position',
					'background-repeat','padding-left',
					'padding-right',	'padding-top',
					'padding-bottom',	'width', 'height');
/* to remember already calculated selectors, by stages */
				$this->restored_selectors = array(
					1 => array(),	2 => array(),	3 => array(),
					4 => array(),	5 => array(),	6 => array(),
					7 => array(),	8 => array(),	9 => array(),
					10 => array(),	11 => array(),	12 => array());
/* try to restore property values from parent selectors */
				foreach ($this->optimizer->media as $import => $images) {
					foreach ($images as $key => $image) {
						foreach ($properties as $property) {
							if (empty($image[$property]) && preg_match("@[a-z\d][#\.\[][^#\.\[]+$@is", $key)) {
								$uniformed_key = $this->fix_css3_selectors($key);
								$this->optimizer->media[$import][$key][$property] = $this->restore_property($import, $uniformed_key, $property);
							}
						}
					}
				}
			}
/* normalize array -- skip items w/o background (none or just color) */
			foreach ($this->optimizer->media as $import => $images) {
				foreach ($images as $key => $image) {
					$back = empty($image['background-image']) ? '' : $image['background-image'];
/* remove quotes from background image */
					if (!empty($back) && ($back{0} == '"' || $back{0} == "'")) {
						$back = substr($back, 1, strlen($back) - 2);
					}
					if (empty($back)) {
/* try to find w/o CSS3 pseudo-selectors, i.e. :focus, :hover, etc */
						$key_fixed = $this->fix_css3_selectors($key);
						if (!empty($this->optimizer->media[$import][$key_fixed])) {
							if (!empty($this->optimizer->media[$import][$key_fixed]['background-image'])) {
								$back = $this->optimizer->media[$import][$key]['background-image'] = $this->optimizer->media[$import][$key_fixed]['background-image'];
								if (empty($image['width']) && !empty($this->optimizer->media[$import][$key_fixed]['width'])) {
									$image['width'] = $this->optimizer->media[$import][$key]['width'] = $this->optimizer->media[$import][$key_fixed]['width'];
								}
								if (empty($image['height']) && !empty($this->optimizer->media[$import][$key_fixed]['height'])) {
									$image['height'] = $this->optimizer->media[$import][$key]['height'] = $this->optimizer->media[$import][$key_fixed]['height'];
								}
								if (empty($image['background-repeat']) && !empty($this->optimizer->media[$import][$key_fixed]['background-repeat'])) {
									$image['background-repeat'] = $this->optimizer->media[$import][$key]['background-repeat'] = $this->optimizer->media[$import][$key_fixed]['background-repeat'];
								}
							}
						}
					}
/* define a few of constants for image */
					$img_has = array();
/* Does image have width? */
					$img_has['width'] = !empty($image['width']);
/* Does image have height? */
					$img_has['height'] = !empty($image['height']);
/* Is image width given in absolute units? */
					$img_has['abs_width'] = $img_has['width'] && !preg_match("/em|%|auto/", $image['width']);
/* Is image height given in absolute units? */
					$img_has['abs_height'] = $img_has['height'] && !preg_match("/em|%|auto/", $image['height']);
/* Does image have background-position? */
					$img_has['position'] = !empty($image['background-position']);
					if ($img_has['position']) {
						$background_position = explode(" ", $image['background-position'], 2);
					} else {
						$background_position = array(0, 0);
					}
					$background_position[1] = empty($background_position[1]) ? 0 : $background_position[1];
/* Is image placed to the right? */
					$img_has['pos_right'] = $img_has['position'] && $background_position[0] == '100%';
/* Is image placed to the bottom? */
					$img_has['pos_bottom'] = $img_has['position'] && $background_position[1] == '100%';
/* Is image placed to the center? */
					$img_has['pos_center'] = $img_has['position'] && ($background_position[0] == '50%' || $background_position[1] == '50%');
/* Does image location depend on container size? */
					$img_has['pos_float'] = $img_has['pos_center'] ||
						($img_has['pos_right'] && preg_match("/%|em/", $background_position[1]) && round($background_position[1])) ||
						($img_has['pos_bottom'] && preg_match("/%|em/", $background_position[0]) && round($background_position[0]));
/* Does image have not absolute position? */
					$img_has['pos_relative'] = $img_has['position'] && preg_match("/right|bottom|center|%|em/", $image['background-position']);
/* Can we calculate background-posititon-x for this image? Thx to Steve Souders */
					$img_has['pos_x_comp'] = strpos($background_position[0], '%') && $img_has['abs_width'];
/* Can we calculate background-posititon-y for this image? */
					$img_has['pos_y_comp'] = strpos($background_position[1], '%') && $img_has['abs_height'];
/* exclude files from ignore list */
					if (!empty($this->optimizer->ignore_list) &&
						in_array(preg_replace("/.*\//", "", substr($back, 4,
							strlen($back) - 5)), $this->optimizer->ignore_list)) {
								unset($this->optimizer->media[$import][$key]);
					}

/* re-check background image existence */
					if (!empty($back) && $back != $this->optimizer->none) {
						if (!empty($image['height']) && !empty($image['width']) && empty($image['background-repeat'])) {
							$image['background-repeat'] = $this->optimizer->media[$import][$key]['background-repeat'] = 'no-repeat';
						}
						if (!empty($image['background-repeat'])) {
							$repeat_key = $image['background-repeat'];
							if ($image['background-repeat'] == 'no-repeat') {
								if ($img_has['abs_height'] && $img_has['pos_right'] && !$img_has['pos_float']) {
									$repeat_key = 'no-repeatr';
								} elseif ($img_has['abs_width'] && $img_has['pos_bottom'] && !$img_has['pos_float']) {
									$repeat_key = 'no-repeatb';
								} elseif (!$img_has['pos_relative']) {
									if (($img_has['abs_width'] && $img_has['abs_height']) || !empty($this->optimizer->aggressive)) {
										$repeat_key = 'no-repeat';
									} else {
										$repeat_key = 'no-repeati';
									}
/* if can't re-calculate background-position for absolute dimensions */
								} elseif (!$img_has['pos_x_comp'] || !$img_has['pos_y_comp']) {
									$repeat_key = 'repeat';
								}
							}
							if ($image['background-repeat'] == 'repeat-x') {
/* if can't re-calculate background-position for absolute dimensions */
								if ($img_has['pos_relative'] && !$img_has['pos_y_comp']) {
									$repeat_key = 'repeat';
								} elseif (!$img_has['abs_height'] && !$this->optimizer->aggressive) {
									$repeat_key = 'repeat-xl';
								}
							}
							if ($image['background-repeat'] == 'repeat-y') {
/* if can't re-calculate background-position for absolute dimensions */
								if ($img_has['pos_relative'] && !$img_has['pos_x_comp']) {
									$repeat_key = 'repeat';
								} elseif (!$img_has['abs_width'] && !$this->optimizer->aggressive) {
									$repeat_key = 'repeat-yl';
								}
							}
/* count selectors for every images -- to handle initial CSS Sprites */
							if (empty($this->optimizer->css_images[$back])) {
								$this->optimizer->css_images[$back] = array();
							}
							$this->optimizer->css_images[$back][empty($image['background-position']) ? 'no' : $image['background-position']] = 1;
/* disable all unsupported cases */
							if ($repeat_key == 'repeat') {
								unset($this->optimizer->media[$import][$key]);
							} else {
								$this->optimizer->media[$import][$key]['background-repeat'] = $repeat_key;
								if (empty($this->optimizer->css_images[$repeat_key])) {
									$this->optimizer->css_images[$repeat_key] = 0;
								}
/* count different images with repeating options */
								$this->optimizer->css_images[$repeat_key]++;
/* count selectors for every images -- to handle initial CSS Sprites */
								if (empty($this->optimizer->css_images[$back])) {
									$this->optimizer->css_images[$back] = array();
								}
								$this->optimizer->css_images[$back][empty($image['background-position']) ? 'no' : $image['background-position']] = 1;
							}
/* disable images w/o background-repeat */
						} else {
							unset($this->optimizer->media[$import][$key]);
						}
					}
				}
/* merge params to form unique string */
				$sorted_selectors = $this->optimizer->media[$import];
/* rearrage by keys -- alphabetically */
				ksort($sorted_selectors);
				foreach ($sorted_selectors as $image) {
					$this->optimizer->timestamp .= (empty($image['background-image']) ? '' :  $image['background-image']) . (empty($image['width']) ? '' :  $image['width']) . (empty($image['height']) ? '' : $image['height']) . (empty($image['background-repeat']) ? '' :  $image['background-repeat']);
				}
				unset($sorted_selectors);
			}
/* convert timestamp to md5 hash */
			$this->optimizer->timestamp = substr(md5($this->optimizer->timestamp), 0, 10);
/* combine dimensional CSS Sprites */
			foreach ($this->optimizer->media as $import => $images) {
				foreach ($images as $key => $image) {
/* no initial CSS Sprites and valid background-image */
					if (!empty($image['background-image']) && $image['background-image'] != $this->optimizer->none && count($this->optimizer->css_images[$image['background-image']]) < 2) {
						$this->sprite = 'webo'. preg_replace("/(repeat-|no-repeat)/", "", $image['background-repeat']) .'.' . $this->optimizer->timestamp .'.png';
						$img = trim(str_replace("!important", "", $image['background-image']));
						$this->optimizer->css_image = substr($img, 4, strlen($img) - 5);
						if ($this->optimizer->css_image{0} == '"' || $this->optimizer->css_image{0} == "'") {
							$this->optimizer->css_image = substr($this->optimizer->css_image, 1, strlen($this->optimizer->css_image) - 2);
						}
						list($width, $height) = $this->optimizer->get_image(0, 0, $this->optimizer->css_image);
/* restrict images by ~64x64 if memory is limited */
						if ($width &&
							$height &&
							(!$this->optimizer->memory_limited ||
								$width * $height < 4097) &&
							(empty($this->optimizer->dimensions_limited) ||
								($width < $this->optimizer->dimensions_limited &&
									$height < $this->optimizer->dimensions_limited))) {
/* fix image dimensions with paddings */
							$image['height'] = (empty($image['height']) ? 0 : round($image['height']))
								+ (empty($image['padding-top']) ? 0 : round($image['padding-top']))
								+ (empty($image['padding-bottom']) ? 0 : round($image['padding-bottom']));
							$image['width'] = (empty($image['width']) ? 0 : round($image['width']))
								+ (empty($image['padding-left']) ? 0 : round($image['padding-left']))
								+ (empty($image['padding-right']) ? 0 : round($image['padding-right']));
/* fix background-position & repeat for fixed images */
							if (!empty($image['width']) &&
								$width == $image['width'] &&
								!empty($image['height']) &&
								$height == $image['height']) {
									$image['background-repeat'] = $this->optimizer->media[$import][$key]['background-repeat'] = 'no-repeat';
/* but try to save existing position */
								if (empty($image['background-position'])) {
									$image['background-position'] = $this->optimizer->media[$import][$key]['background-position'] = '0 0';
								}
							}
/* calculate backround-position for image with relative position but absolute dimensions */
							if (!empty($image['background-position']) &&
								strpos($image['background-position'], '%')) {
									$width_real = max($width, $image['width']);
									$height_real = max($height, $image['height']);
									$position = explode(" ", $image['background-position'], 2);
									$position_x = round(round($position[0]) * ($image['width'] - $width) / 100);
									$position_x .= ($position_x ? 'px' : '');
									$position_y = round(round($position[1]) * ($image['height'] - $height) / 100);
									$position_y .= ($position_y ? 'px' : '');
									switch ($image['background-repeat']) {
										case 'no-repeat':
											$position[0] = $position_x;
										case 'repeat-x':
											$position[1] = $position_y;
											break;
										case 'repeat-y':
											$position[0] = $position_x;
											break;
									}
									$image['background-position'] =  $this->optimizer->media[$import][$key]['background-position'] = implode(" ", $position);
							}
							if (empty($this->optimizer->css_images[$this->sprite])) {
								$this->optimizer->css_images[$this->sprite] = array();
								$this->optimizer->css_images[$this->sprite]['x'] = 0;
								$this->optimizer->css_images[$this->sprite]['y'] = 0;
								$this->optimizer->css_images[$this->sprite]['images'] = array();
							}
/* fast fix for recalculating Sprites from PNG to JPEG -- don't touch files themselves */
							if (preg_match("/\.jpe?g$/i", $this->optimizer->css_image) && $this->optimizer->truecolor_in_jpeg) {
								$this->optimizer->css_images[$this->sprite]['jpeg'] = 1;
							}
							$shift_x = $shift_y = $top = $left = 0;
							$position = empty($image['background-position']) ? array(0, 0) : explode(" ", $image['background-position'], 2);
							switch ($image['background-repeat']) {
/* repeat-x case w/ dimensions */
								case 'repeat-x':
/* repeat-x case w/o dimensions - can be added safely only to the end of Sprite */
								case 'repeat-xl':
									$top = round($position[1]);
/* shift for bottom left corner of the object */
									$shift_y = $image['height'] > $height ? $image['height'] - $height : 0;
									break;
/* repeat-y case */
								case 'repeat-y':
/* repeat-y case w/o dimensions - can be added safely only to the end of Sprite */
								case 'repeat-yl':
									$left = round($position[0]);
									$shift_x = $image['width'] > $width ? $image['width'] - $width : 0;
									break;
/* no-repeat case w/ dimensions can be placed all together */
								case 'no-repeat':
									$shift_x = $image['width'] > $width ? $image['width'] - $width : 0;
									$shift_y = $image['height'] > $height ? $image['height'] - $height : 0;
/* cut from initial image area marked with CSS rules */
									$width = $image['width'] < $width ? $image['width'] : $width;
									$height = $image['height'] < $height ? $image['height'] : $height;
/* no-repeat case w/o dimensions -- icons -- should be placed like this:
	*
   *
  *
 *
*/
								case 'no-repeati':
/* don't need any shift for icons -- they have enough room for any object */
									$left = round($position[0]);
									$top = round($position[1]);
									break;
/* no-repeat case with 100% 0 */
								case 'no-repeatr':
									$left = 'right';
									$top = round($position[1]);
									$shift_y = $image['height'] > $height ? $image['height'] - $height : 0;
									break;
/* no-repeat case with 0 100% */
								case 'no-repeatb':
									$left = round($position[0]);
									$top = 'bottom';
									$shift_y = $image['width'] > $height ? $image['width'] - $height : 0;
									break;
								}
/* add image to CSS Sprite to merge. Overall picture looks like this:
__________________
|    left         |   <- outer borders belong to
|top  ______      |      an object
|    | width|     |    <- inner borders belong
|    |height|     |       to the image
|    |______|     |
|            shift|
|_________________|
*/
							$this->optimizer->css_images[$this->sprite]['images'][] = array($this->optimizer->css_image, $width, $height, $left, $top, $shift_x, $shift_y, $import, $key);
						}
					}
				}
			}
/* merge simple cases: repeat-x/y */
			$this->optimizer->merge_sprites(1, 'webox.'. $this->optimizer->timestamp .'.png');;
			$this->optimizer->merge_sprites(2, 'weboy.'. $this->optimizer->timestamp .'.png');
/* handle some specific cases -- 0 100% and 100% 0 images */
			$this->optimizer->merge_sprites(5, 'webor.'. $this->optimizer->timestamp .'.png');
			$this->optimizer->merge_sprites(6, 'webob.'. $this->optimizer->timestamp .'.png');
		}
/* create first part of CSS Sprites */
		if ($this->optimizer->partly || empty($this->optimizer->css)) {
			return array('', '');
		} else {
			if (empty($this->optimizer->no_sprites)) {
/* only then try to combine all possible images into the last one */
				$this->optimizer->merge_sprites(4, 'webo.'. $this->optimizer->timestamp .'.png');
/* delete temporary files */
				if (is_file('webor.'. $this->optimizer->timestamp .'.png')) {
					@unlink('webor.'. $this->optimizer->timestamp .'.png');
				}
				if (is_file('webob.'. $this->optimizer->timestamp .'.png')) {
					@unlink('webob.'. $this->optimizer->timestamp .'.png');
				}
			}
/* finally convert CSS images to data:URI and add mutiple hosts, or add static proxy */
			if (!empty($this->optimizer->data_uris) ||
				!empty($this->optimizer->multiple_hosts_count) ||
				!empty($this->proxy) ||
				!empty($this->optimizer->proxy_rewrite)) {
					$this->css_to_data_uri();
			}
/* after 0.6.2 return array of separated files */
			return array(html_entity_decode($this->optimizer->css->print->formatted(), ENT_QUOTES), $this->optimizer->compressed_mhtml . ($this->optimizer->ie && $this->optimizer->separated ? "\n*/" : ""));
		}
	}
/* convert all CSS images to base64 */
	function css_to_data_uri () {
/* location for mhtml */
		$location = 0;
		$general_access = !empty($this->optimizer->mhtml) ||
			!empty($this->optimizer->proxy_rewrite) ||
			!empty($this->multiple_hosts);
		foreach ($this->optimizer->css->css as $import => $token) {
/* open @media definition*/
			if (!empty($this->optimizer->separated) && !$this->optimizer->ie && !round($import)) {
				$this->optimizer->compressed_mhtml .= '@media ' . $import . '{';
			}
			foreach ($token as $tags => $rule) {
/* skip IE6/7 hacks */
				if ($general_access ||
					(strpos($tags, '* html') === false &&
					strpos($tags, '*+html') === false)) {
						foreach ($rule as $key => $value) {
/* standartize all background values from input */
							if (strpos(" " . $key, "background")) {
								$background = array();
								if ($key == 'background') {
/* resolve background property */
									$background = $this->optimizer->css->optimise->dissolve_short_bg($value);
								} else {
/* skip default properties */
									$background[$key] = $value;
								}
								if (!empty($background['background-image'])) {
									$image = trim(str_replace("!important", "", $background['background-image']));
									$this->optimizer->css_image = substr($image, 4, strlen($image) - 5);
									if ($this->optimizer->css_image{0} == '"' ||
										$this->optimizer->css_image{0} == "'") {
											$this->optimizer->css_image =
												substr($this->optimizer->css_image, 1,
												strlen($this->optimizer->css_image) - 2);
									}
									if (!empty($this->optimizer->css_image)) {
										$sprited =
											strpos($this->optimizer->css_image,
											'ebo.' . $this->optimizer->timestamp);
										if (!empty($this->optimizer->data_uris) &&
											!$this->optimizer->ie &&
											!preg_match("@^\*(\s|\+)\s*html@s", $tags)) {
/* convert image to data:URI */
												$this->optimizer->css_image =
													$this->optimizer->get_image(1, 0,
													$this->optimizer->css_image);
										} elseif (!empty($this->optimizer->data_uris) &&
											!empty($this->optimizer->mhtml) &&
											$this->optimizer->ie &&
											!$this->optimizer->ie7v) {
/* convert image to mhtml: */
												$this->optimizer->css_image =
													$this->optimizer->get_image(2,
													$location++,
													$this->optimizer->css_image);
										}
										if (substr($this->optimizer->css_image, 0, 5) !== 'data:' &&
											substr($this->optimizer->css_image, 0, 6) !== 'mhtml:') {
/* skip images on different hosts */
											$this->optimizer->css_image =
												$this->distribute_image($this->optimizer->css_image);
										}
/* add quotes for background images with spaces */
										if (strpos($this->optimizer->css_image, ' ')) {
											$this->optimizer->css_image = "'" . $this->optimizer->css_image . "'";
										}
/* separate background-image rules from the others? */
										if (empty($this->optimizer->separated)) {
											$this->optimizer->css->css[$import][$tags][$key] =
												preg_replace("/url\([^\)]+\)(\s*)?/", "url(" .
												$this->optimizer->css_image .
												")$1", $value);
										} else {
/* add for IE add call to mhtml resource file */
											if ($this->optimizer->ie) {
												$this->optimizer->css->css[$import][$tags][$key] =
													preg_replace("/url\([^\)]+\)(\s*)/", "url(" .
													$this->optimizer->css_image . ")$1", $value);	
/* for others just remove background-image call */
											} else {
												$this->optimizer->css->css[$import][$tags][$key] =
													preg_replace("/url\([^\)]+\)/", "", $value);
												$this->optimizer->compressed_mhtml .=
													$tags . '{background-image:url(' .
													$this->optimizer->css_image . ')!important}';
											}
/* skip empty background-image */
											if (empty($this->optimizer->css->css[$import][$tags][$key])) {
												unset($this->optimizer->css->css[$import][$tags][$key]);
											}
											if (!count($this->optimizer->css->css[$import][$tags])) {
												unset($this->optimizer->css->css[$import][$tags]);
											}
										}
									}
								}
							}
						}
				}
			}
/* close @media definition*/
			if (!empty($this->optimizer->separated) && !$this->optimizer->ie && !round($import)) {
				$this->optimizer->compressed_mhtml .= '}';
			}
		}
	}
/* cdistribute image through multiple hosts */
	function distribute_image ($image) {
		if (!empty($this->optimizer->multiple_hosts_count) &&
			(!strpos($image, "://") ||
				preg_match("/:\/\/(www\.)?" . preg_replace("/^www\./", "", $_SERVER['HTTP_HOST']) . "\//i", $image))) {
/* add absolute path for sprited images */
					if (0 === strpos($image, 'webo')) {
						$image = str_replace($this->optimizer->website_root, "/", $this->optimizer->current_dir) . $image;
					}
					$host = $this->optimizer->multiple_hosts[strlen($image)%$this->optimizer->multiple_hosts_count];
/* if we have dot in the distribution host - it's a domain name */
					$new_host = $host .
						((strpos($host, '.') === false) ?
						'.' . preg_replace("/^www\./", "", $_SERVER['HTTP_HOST']): '');
					return "http" . $this->optimizer->https . "://" . $new_host . $image;
		} elseif (!empty($this->optimizer->proxy_rewrite)) {
/* add absolute path for sprited images */
			if (0 === strpos($image, 'webo')) {
				$image = str_replace($this->optimizer->website_root, "/", $this->optimizer->current_dir) . $image;
			}
			if (!strpos($image, "://") && preg_match("@\.(bmp|gif|png|ico|jpe?g)$@i", $image)) {
/* do not touch dynamic images -- how we can handle them? */
				$image = $this->optimizer->html_dir . 'wo.static.php?' . $image;
			}
			return $image;
		} else {
			return $image;
		}
	}
/* return CSS selector w/o CSS3 pseudo-addons */
	function fix_css3_selectors ($key) {
		$uniformed_key = $key;
		if (($semicolon = strpos($uniformed_key, ':')) !== false) {
			$space = strpos($uniformed_key, ' ', $semicolon);
			$space = $space ? $space : strlen($uniformed_key);
			$uniformed_key = substr_replace($uniformed_key, '', $semicolon, $space - $semicolon);
		}
		return $uniformed_key;
	}
/* try to restore CSS property from some parent selectors for a given one */
	function restore_property ($import, $selector, $property, $stage = 1) {
		switch ($stage) {
/* remove all attribute selectors */
			case 1:
				$regexp = '([a-z\d]+)\[[^\[]+\]$';
				$part = '$1';
				break;
/* remove all class selectors */
			case 2:
				$regexp = '([a-z\d]+)\.[^\.\s]+$';
				$part = '$1';
				break;
/* remove all identificator selectors */
			case 3:
				$regexp = '([a-z\d]+)#[^#\s]+$';
				$part = '$1';
				break;
/* remove 1 attribute selectors from start */
			case 4:
				$regexp = '^([a-z\d]+)\[[^\[]+\]\s([a-z\d]+)';
				$part = '$1 $2';
				break;
/* remove 1 class selectors from start */
			case 5:
				$regexp = '^([a-z\d]+)\.[^\.\s]+\s([a-z\d]+)';
				$part = '$1 $2';
				break;
/* remove 1 identificator selectors from start */
			case 6:
				$regexp = '^([a-z\d]+)#[^#\s]+\s([a-z\d]+)';
				$part = '$1 $2';
				break;
/* remove all attribute selectors */
			case 7:
				$regexp = '([a-z\d]+)\[[^\[]+\]';
				$part = '$1';
				break;
/* remove all class selectors */
			case 8:
				$regexp = '([a-z\d]+)\.[^\.\s]+';
				$part = '$1';
				break;
/* remove all identificator selectors */
			case 9:
				$regexp = '([a-z\d]+)#[^#\s]+';
				$part = '$1';
				break;
/* remove the first tag */
			case 10:
				$regexp = '^\S+\s';
				$part = '';
				break;
/* remove the first 2 tags */
			case 11:
				$regexp = '^\S+\s\S+\s';
				$part = '';
				break;
/* remove the first 3 tags */
			case 12:
				$regexp = '^\S+\s\S+\s\S+\s';
				$part = '';
				break;

/* already have removed all possibilities, exit */
			case 13:
				$regexp = null;
				break;
		}
		if (!empty($regexp)) {
			$restored_selectors = array();
			if (empty($this->restored_selectors[$stage][$selector])) {
/* clear selector for current stage */
				$restored_selector = preg_replace("@" . $regexp . "@is", $part, $selector);
			} else {
/* or get from calculated ones */
				$restored_selectors = $this->restored_selectors[$stage][$selector];
				if (is_array($restored_selectors)) {
					$restored_selector = $restored_selectors[0];
				} else {
					$restored_selector = $selector;
				}
			}
			if ($restored_selector != $selector) {
				if (!count($restored_selectors)) {
/* just copy existing element to this array */
					if (!empty($this->optimizer->css->css[$import][$restored_selector])) {
						$restored_selectors[] = $restored_selector;
					}
/* and try to find any selectors containing calculated one */
					$selectors = array_keys($this->optimizer->css->css[$import]);
					foreach ($selectors as $possible_selector) {
						if (preg_match("@(^|,)" . $restored_selector . "(,|$)@", $possible_selector)) {
							$restored_selectors[] = $possible_selector;
						}
					}
				}
/* there is no selectors in restored array */
				if (is_array($restored_selectors) && count($restored_selectors)) {
/* loop in all restored selectors */
					foreach ($restored_selectors as $restored_selector) {
/* try to resolve background shorthand */
						if (strpos($property, "ackground") &&
							empty($this->optimizer->css->css[$import][$restored_selector][$property]) &&
							!empty($this->optimizer->css->css[$import][$restored_selector]['background'])) {
								$background = $this->optimizer->css->optimise->dissolve_short_bg($this->optimizer->css->css[$import][$restored_selector]['background']);
/* in resolved property these is no give key */
								if (!empty($background[$property])) {
									$return = $background[$property];
								}
/* try to resolve padding shorthand */
						} elseif (strpos($property, "adding") &&
							empty($this->optimizer->css->css[$import][$restored_selector][$property]) &&
							!empty($this->optimizer->css->css[$import][$restored_selector][$property]['padding'])) {
								$padding = $this->optimizer->css->optimise->dissolve_4value_shorthands('padding', $this->optimizer->css->css[$import][$restored_selector][$property]['padding']);
/* in resolved property these is no given key */
								if (!empty($padding[$property])) {
									$return = $padding[$property];
								}
/* property is defined w/o shorthands */
						} elseif (!empty($this->optimizer->css->css[$import][$restored_selector][$property])) {
							$return = $this->optimizer->css->css[$import][$restored_selector][$property];
						}
					}
/* remember restored selectors for future properties */
					if (empty($this->restored_selectors[$stage][$selector])) {
						$this->restored_selectors[$stage][$selector] = $restored_selectors;
					}
				}
			}
			if (empty($this->restored_selectors[$stage][$selector])) {
				$this->restored_selectors[$stage][$selector] = 'No';
			}
			if (empty($return)) {
				$return = $this->restore_property($import, $selector, $property, ++$stage);
			}
		} else {
			$return = null;
		}
		return $return;
	}
/* fix background-position:	left|right|center -> 0 50%|100% 50%|50% 50%
							top|bottom -> 50% 0|50% 100% */
	function compute_background_position ($value) {
/* step 1: restore half-value to full one */
		$values = explode(" ", $value, 2);
		if (!isset($values[1])) {
			switch ($values[0]) {
				case 'top':
				case 'bottom':
					$value = '50% ' . $values[0];
					break;
				default:
					$value = $values[0] . ' 50%';
					break;
			}
		}
/* step 2: convert 0px etc to absolute 0 */
		$values = explode(" ", $value);
		if (!empty($values[0]) && $values[0]{0} == '0') {
			$values[0] = 0;
		}
		if (!empty($values[1]) && substr($values[1], 0, 1) == '0') {
			$values[1] = 0;
		}
/* step 3: switch bottom left to left bottom etc 
			switch 40px left to left 40px etc */
		if (in_array($values[0], array('top, bottom')) || in_array($values[1], array('left', 'right'))) {
			$values = array_reverse($values);
		}
		$value = $values[0] . " " . $values[1];
/* step 4: replace words with percent */
		$value = str_replace(array('left', 'top', 'center', 'bottom', 'right'), array(0, 0, '50%', '100%', '100%'), $value);
		return $value;
	}
/* delete !important and trim spaces from given property */
	function normalize_property ($value) {
		return trim(str_replace("!important", "", $value));
	}

}

?>