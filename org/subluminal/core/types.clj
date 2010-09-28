(in-ns 'org.subluminal.xproto)

; X type aliases
(derive ::card16 ::bin/uint16)
(derive ::card32 ::bin/uint32)
(derive ::card8  ::bin/uint8)

(derive ::cursor   ::bin/int32) ; top 3 bits guaranteed zero
(derive ::colormap ::bin/int32)

(derive ::drawable ::bin/int32)
(derive ::window   ::drawable)
(derive ::pixmap   ::drawable)

(derive ::fontable ::bin/int32)
(derive ::font     ::fontable)
(derive ::gcontext ::fontable)

(derive ::atom ::bin/int32)

(comment
(defmethod bin/read-binary ::atom
  [tag buf]
  (let [atom-id (int (bin/read-binary ::bin/int32 buf))]
    (get @(:atoms-lookup *display*) atom-id)))

(defmethod bin/write-binary ::atom
  [tag buf kw]
  (bin/write-binary ::bin/int32 buf
                    (get @(:atoms *display*) kw))))

(derive ::visual-id ::bin/int32)

(derive ::timestamp ::card32)
(derive ::keysym    ::bin/int32)
(derive ::keycode   ::card8)
(derive ::button    ::card8)

(bin/defbinary bitgravity
  [internal ::card8 {:xenum {:forget 0, :nw 1, :n 2, :ne 3, :w 4, :center 5,
                             :e 6, :sw 7, :s 8, :se 9, :static 10}}])

(bin/defbinary wingravity
  [internal ::card8 {:xenum {:unmap 0 :static 10
                             :nw 1 :n 2 :ne 3 :w 4 :center 5
                             :e 6 :sw 7 :s 8 :se 9}}])
(bin/defbinary device-event
  (internal ::card16 {:bitmask
            {:key-press 0
             :key-release 1
             :button-press 2
             :button-release 3
             :enter-window 4
             :leave-window 5
             :pointer-motion 6
             :pointer-motion-hint 7
             :button1-motion 8
             :button2-motion 9
             :button3-motion 10
             :button4-motion 11
             :button5-motion 12
             :button-motion 13}}))

(bin/defbinary eventmask
  (internal ::card32 {:bitmask
            {:key-press      0
             :key-release    1
             :button-press   2
             :button-release 3
             :enter-window   4
             :leave-window   5
             :pointer-motion      6
             :pointer-motion-hint 7
             :button1-motion 8
             :button2-motion 9
             :button3-motion 10
             :button4-motion 11
             :button5-motion 12
             :button-motion  13
             :keymap-state   14
             :exposure       15
             :visibility-change 16
             :structure-notify  17
             :resize-redirect   18
             :substructure-notify 19
             :substructure-redirect 20
             :focus-change 21
             :property-change 22
             :colormap-change 23
             :owner-grab-button 24}}))

(bin/defbinary keybut-mask
  (internal ::card16 {:bitmask {:shift 0 :lock 1 :control 2
                                :mod1 3 :mod2 4 :mod3 5 :mod4 6 :mod5 7
                                :button1 8 :button2 9 :button3 10 :button4 11
                                :button5 12
                                :any-modifier 15}}))

(bin/defbinary point
  [:x ::bin/int16]
  [:y ::bin/int16])

(bin/defbinary rect
  [:x ::bin/int16]
  [:y ::bin/int16]
  [:w ::card16]
  [:h ::card16])

(bin/defbinary arc
  [:x ::bin/int16]
  [:y ::bin/int16]
  [:w ::card16]
  [:h ::card16]
  [:angle1 ::bin/int16]
  [:angle2 ::bin/int16])

(bin/defbinary host
  (:family ::card8 {:enum {:internet 0, :decnet 1, :chaos 2,
                           :server-interpreted 5, :ipv6 6}})
  (skip 1)
  (:address-len ::card16 {:aux (count (:address host))})
  (:address ::card8 {:times address-len})
  (align 4))

(def +predefined-atoms+
     {:PRIMARY 1 :SECONDARY 2 :ARC 3 :ATOM 4 :BITMAP 5
      :CARDINAL 6 :COLORMAP 7 :CURSOR 8 :CUT_BUFFER0 9
      :CUT_BUFFER1 10 :CUT_BUFFER2 11 :CUT_BUFFER3 12
      :CUT_BUFFER4 13 :CUT_BUFFER5 14 :CUT_BUFFER6 15
      :CUT_BUFFER7 16 :DRAWABLE 17 :FONT 18 :INTEGER 19
      :PIXMAP 20 :POINT 21 :RECTANGLE 22
      :RESOURCE_MANAGER 23 :RGB_COLOR_MAP 24
      :RGB_BEST_MAP 25 :RGB_BLUE_MAP 26
      :RGB_DEFAULT_MAP 27 :RGB_GRAY_MAP 28
      :RGB_GREEN_MAP 29 :RGB_RED_MAP 30 :STRING 31
      :VISUALID 32 :WINDOW 33 :WM_COMMAND 34
      :WM_HINTS 35 :WM_CLIENT_MACHINE 36
      :WM_ICON_NAME 37 :WM_ICON_SIZE 38 :WM_NAME 39
      :WM_NORMAL_HINTS 40 :WM_SIZE_HINTS 41
      :WM_ZOOM_HINTS 42 :MIN_SPACE 43 :NORM_SPACE 44
      :MAX_SPACE 45 :END_SPACE 46 :SUPERSCRIPT_X 47
      :SUPERSCRIPT_Y 48 :SUBSCRIPT_X 49 :SUBSCRIPT_Y 50
      :UNDERLINE_POSITION 51 :UNDERLINE_THICKNESS 52
      :STRIKEOUT_ASCENT 53 :STRIKEOUT_DESCENT 54
      :ITALIC_ANGLE 55 :X_HEIGHT 56 :QUAD_WIDTH 57
      :WEIGHT 58 :POINT_SIZE 59 :RESOLUTION 60
      :COPYRIGHT 61 :NOTICE 62 :FONT_NAME 63
      :FAMILY_NAME 64 :FULL_NAME 65 :CAP_HEIGHT 66
      :WM_CLASS 67 :WM_TRANSIENT_FOR 68})

(def +core-ops+
     {::create-window 1, ::change-window-attributes 2,
      ::get-window-attributes 3, ::destroy-window 4,
      ::destroy-subwindows 5, ::change-save-set 6,
      ::reparent-window 7, ::map-window 8, :map-subwindows 9,
      ::unmap-window 10, ::unmap-subwindows 11,
      ::configure-window 12, ::circulate-window 13,
      ::get-geometry 14, ::query-tree 15,

      ::intern-atom 16, ::get-atom-name 17,

      ::change-property 18, ::delete-property 19,
      ::get-property 20, ::list-properties 21,

      ::set-selection-owner 22, ::get-selection-owner 23,
      ::convert-selection 24,

      ::send-event 25,

      ::grab-pointer 26, ::ungrab-pointer 27,
      ::grab-button 28, ::ungrab-button 29,
      ::change-active-pointer-grab 30,
      ::grab-keyboard 31, ::ungrab-keyboard 32,
      ::grab-key 33, ::ungrab-key 34,
      ::allow-events 35, ::grab-server 36, ::ungrab-server 37,

      ::query-pointer 38, ::get-motion-events 39,
      ::translate-coordinates 40, ::warp-pointer 41,

      ::set-input-focus 42, ::get-input-focus 43,

      ::query-keymap 44,

      ::open-font 45, ::close-font 46, ::query-font 47,
      ::query-text-extents 48, ::list-fonts 49,
      ::list-fonts-with-info 50,
      ::set-font-path 51, ::get-font-path 52,

      ::create-pixmap 53, ::free-pixmap 54,

      ::create-gc 55, ::change-gc 56, ::copy-gc 57,
      ::set-dashes 58, ::set-clip-rectangles 59,
      ::free-gc 60,

      ::clear-area 61, ::copy-area 62, ::copy-plane 63,
      ::poly-point 64, ::poly-line 65, ::poly-segment 66,
      ::poly-rectangle 67, ::poly-arc 68,
      ::fill-poly 69, ::poly-fill-rectangle 70,
      ::poly-fill-arc 71,
      
      ::put-image 72, ::get-image 73,
      ::poly-text8 74, ::poly-text16 75,
      ::image-text8 76, ::image-text16 77,

      ::create-colormap 78, ::free-colormap 79,
      ::copy-colormap-and-free 80, ::install-colormap 81,
      ::uninstall-colormap 82, ::list-installed-colormaps 83,
      ::alloc-color 84, ::alloc-named-color 85,
      ::alloc-color-cells 86, ::alloc-color-planes 87,
      ::free-colors 88, ::store-colors 89,
      ::store-named-color 90, ::query-colors 91,
      ::lookup-color 92,
      
      ::create-cursor 93, ::create-glyph-cursor 94,
      ::free-cursor 95, ::recolor-cursor 96,
      ::query-best-size 97,

      ::query-extension 98, ::list-extensions 99,

      ::change-keyboard-mapping 100, ::get-keyboard-mapping 101,
      ::change-keyboard-control 102, ::get-keyboard-control 103, ::bell 104,
      ::change-pointer-control 105, ::get-pointer-control 106,

      ::set-screen-saver 107, ::get-screen-saver 108,

      ::change-hosts 109, ::list-hosts 110,
      ::set-access-control 111,
      ::set-close-down-mode 112, ::kill-client 113,

      ::rotate-properties 114,
      ::force-screen-saver 115,

      ::set-pointer-mapping 116, ::get-pointer-mapping 117,
      ::set-modifier-mapping 118, ::get-modifier-mapping 119,

      ::no-operation 127})

(def +core-ops-lookup+ (bin/invert-map +core-ops+))


(defmethod bin/read-binary ::ascii
  ([_ buf]
     (bin/read-binary ::ascii buf (bin/read-binary ::card8 buf)))
  ([_ buf len]
     (apply str
	    (repeatedly len
			(fn []
			  (char (bin/read-binary ::card8 buf)))))))

(defmethod bin/write-binary ::ascii
  ([_ buf obj] (bin/write-binary ::card8 buf (count obj))
               (doseq [ch obj] (bin/write-binary ::card8 buf (int ch))))
  ([_ buf obj len]
    (doseq [ch obj] (bin/write-binary ::card8 buf (int ch)))))

(defmethod bin/write-binary ::string16
  ;; XXX
  ([_ buf obj] (bin/write-binary ::string16 buf obj 0))
  ([_ buf obj n]
   (doseq [ch obj] (bin/write-binary ::card16 buf (int ch)))))

(defmethod  bin/read-binary ::string16
  ([_ buf obj n]
     (apply str
       (repeatedly n
         (fn []
           (char (bin/read-binary ::card16 buf)))))))
