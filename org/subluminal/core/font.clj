(in-ns 'org.subluminal.xproto)

(define-core-op
  (::open-font (+ 3 (/ (bin/pad4 (count (:name open-font))) 4))
    (skip 1)
    [:fid ::font {:aux *alloc-resource*}]
    [:name-len ::card16 {:aux (count (:name open-font))}]
    (skip 2)
    [:name [::ascii (count (:name open-font))]]
    (align 4)))

(define-core-op
  (::close-font 2
    (skip 1)
    [:font ::font]))

(define-core-op
  (::query-font 2
    (skip 1)
    [:font ::fontable])
  (::query-font-reply
    [:unused ::card8 {:aux 0}]
    [:min-bounds ::charinfo]
    (skip 4)
    [:max-bounds ::charinfo]
    (skip 4)
    [:min-char ::card16]
    [:max-char ::card16]
    [:default-char ::card16]
    [:num-props ::card16 {:aux 0}]
    [:draw-direction ::card8 {:enum {:left-to-right 0 :right-to-left 1}}]
    [:min-byte1 ::card8]
    [:max-byte1 ::card8]
    [:all-chars-exist ::card8]
    [:font-ascent ::bin/int16]
    [:font-descent ::bin/int16]
    [:num-charinfo ::card32 ]
    [:properties ::fontprop {:times num-props}]
    [:char-infos ::charinfo {:times num-charinfo}]))

(bin/defbinary fontprop
  [:name ::atom]
  [:value ::card32])

(bin/defbinary charinfo
  [:left-side-bearing ::bin/int16]
  [:right-side-bearing ::bin/int16]
  [:character-width ::bin/int16]
  [:ascent ::bin/int16]
  [:descent ::bin/int16]
  [:attributes ::card16])

(define-core-op
  (::query-text-extents (-> query-text-extents :text count
                            (* ,, 2) (/ ,, 4) (+ ,, 2))
    [:font ::fontable]
    [:text ::string16]
    (align 4))
  (::query-text-extents-reply
    [:draw-direction ::card8 {:xenum {:left-to-right 0 :right-to-left 1}}]
    [:font-ascent ::bin/int16]
    [:font-descent ::bin/int16]
    [:overall-ascent ::bin/int16]
    [:overall-descent ::bin/int16]
    [:overall-width ::bin/int32]
    [:overall-left ::bin/int32]
    [:overall-right ::bin/int32]
    (skip 4)))

(define-core-op
  (::list-fonts (-> list-fonts :pattern count bin/pad4 (/ 4) (+ 2))
    (skip 1)
    [:max-names ::card16]
    [:patsize ::card16 {:aux (count (:pattern list-fonts))}]
    [:pattern ::ascii]
    (align 4))
  (::list-fonts-reply
    [:unused ::card8 {:aux 0}]
    [:name-count ::card16 {:aux 0}]
    (skip 22)
    [:names ::ascii {:times name-count}]
    (align 4)))
