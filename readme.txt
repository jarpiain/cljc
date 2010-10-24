 * Copyright (c) Juha Arpiainen. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.

This distribution contains the following libs:

org.subluminal.compiler
=======================

A clojure port of clojure.lang.Compiler by Rich Hickey,
aiming for compatibility with the 1.3 alphas.
Does not depend on ASM (see below). Retargetting to ASM
if desired shouldn't be too hard.

This addresses some of the issues mentioned in
http://dev.clojure.org/display/design/Compiler+in+Clojure

Example:

(require '(org.subluminal [compiler :as cljc]))
(cljc/compile 'some.lib)
(cljc/eval '(try (do-something) (catch Exception e "oops")))

Two passes are implemented by the analyze and gen multimethods.
Gen is partially lazy and uses no mutable state (analyze uses
1 bit per local binding reference).

Try (cljc/analyze-fragment form :expression nil)
and (cljc/gen-fragment form :return true)
to sample output of the passes. The second argument
should be one of :expression, :statement, :return or :eval.
If the third argument is 'true or a primitive Class object,
a primitive result may be generated.

Set cljc/*debug-inspect* to true to view compiled classes in
a clojure tree inspector. (Warning! Will open a JFrame for each
compiled class)

Missing/broken features (partial list!)
- Generates no debug info whatsoever
- deftype*, reify*, letfn*, and set! are not implemented
- case* is translated into a sequence of ifs resulting
  in O(n) lookup.
- No optimization for keyword and var fn invocations

Seems to work after a 3-stage bootstrap of itself
and the binfmt and class-file libs. (Comparing the second
and third stages is non-trivial. Gensyms are used for class
and field names so the generated classes will not be bit-for-bit
equal). Remember to make a copy of the code in another namespace
and use cljc/load-file instead of :use or :require. I haven't yet
tried compiling any part of core.clj.

org.subluminal.binfmt
=====================

A library for parsing and writing data in structured binary formats
based around java.nio.ByteBuffer.

Example:

(require '(org.subluminal [binfmt :as bin]))

(bin/defbinary vect2d
  [:x ::bin/int16]
  [:y ::bin/int16])

(bin/defbinary circle2d
  [:center ::vect2d]
  [:radius ::bin/uint16])

(bin/read-binary ::vect2d (bin/buffer-wrap [0 10 0 20]))
--> {:x 10 :y 20} ; ByteBuffers are big-endian by default

(bin/read-binary ::circle2d (bin/buffer-wrap [-1 -5 0 0 1 0]))
--> {:radius 256 :center {:x -5 :y 0}}

(let [buf (ByteBuffer/allocate 4)]
  (bin/write-binary ::vect2d {:x 1 :y 2})
  (seq (.array buf)))
--> (0 1 0 2)

Defbinary has options for enums, bitmasks and constraints on fields.
Iteration is supported with the :times option. if, cond, and case can
be used in defbinary bodies to provide conditional parsing and writing.
The class-file library demonstrates most of the options (compare the
ClassFile definition in class_file.clj with the JVM specification at
http://java.sun.com/docs/books/jvms/second_edition/html/ClassFile.doc.html ).

Reader and writer fns can be provided directly with defprimitive:

(bin/defprimitive my-int16 [buf ^Number obj]
  (.getShort buf)
  (.putShort buf (short obj)))

The internal representation of a format defined this way can be
any clojure object (there's no need for explicit tagging).
Formats defined with defbinary have a hash-map as their internal
representation.

org.subluminal.class-file
=========================

A library for assembling and disassembling bytecode and .class files.
Try e.g.

(clojure.inspector/inspect-tree
  (org.subluminal.class-file/parse-class Object))

to view disassembler output. The .class file must exist somewhere
on the classpath for this to work.
