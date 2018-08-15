(in-package :cl-jupyter-widgets)

(defparameter *color-names* (list "aliceblue" "antiquewhite" "aqua" "aquamarine" "azure" "beige" "bisque" "black" "blanchedalmond" "blue" "blueviolet" "brown" "burlywood" "cadetblue" "chartreuse" "chocolate" "coral" "cornflowerblue" "cornsilk" "crimson" "cyan" "darkblue" "darkcyan" "darkgoldenrod" "darkgray" "darkgreen" "darkkhaki" "darkmagenta" "darkolivegreen" "darkorange" "darkorchid" "darkred" "darksalmon" "darkseagreen" "darkslateblue" "darkslategray" "darkturquoise" "darkviolet" "deeppink" "deepskyblue" "dimgray" "dodgerblue" "firebrick" "floralwhite" "forestgreen" "fuchsia" "gainsboro" "ghostwhite" "gold" "goldenrod" "gray" "green" "greenyellow" "honeydew" "hotpink" "indianred " "indigo " "ivory" "khaki" "lavender" "lavenderblush" "lawngreen" "lemonchiffon" "lightblue" "lightcoral" "lightcyan" "lightgoldenrodyellow" "lightgray" "lightgreen" "lightpink" "lightsalmon" "lightseagreen" "lightskyblue" "lightslategray" "lightsteelblue" "lightyellow" "lime" "limegreen" "linen" "magenta" "maroon" "mediumaquamarine" "mediumblue" "mediumorchid" "mediumpurple" "mediumseagreen" "mediumslateblue" "mediumspringgreen" "mediumturquoise" "mediumvioletred" "midnightblue" "mintcream" "mistyrose" "moccasin" "navajowhite" "navy" "oldlace" "olive" "olivedrab" "orange" "orangered" "orchid" "palegoldenrod" "palegreen" "paleturquoise" "palevioletred" "papayawhip" "peachpuff" "peru" "pink" "plum" "powderblue" "purple" "rebeccapurple" "red" "rosybrown" "royalblue" "saddlebrown" "salmon" "sandybrown" "seagreen" "seashell" "sienna" "silver" "skyblue" "slateblue" "slategray" "snow" "springgreen" "steelblue" "tan" "teal" "thistle" "tomato" "transparent" "turquoise" "violet" "wheat" "white" "whitesmoke" "yellow" "yellowgreen"))

(defclass color ()
  ((info-text :accessor info-text
	      :type string
	      :initform "a valid HTML color"
	      :validator color-validate))
  (:metaclass traitlets:traitlet-class))

(defmethod color-validate ((self color) obj value)
  (if (member value *color-names* :test #'string-equal)
      (string-downcase value)
      (error "~a is not a valid color" value)))

(defclass instance-dict ()
  ((instance :initarg :instance :accessor instance)))


#||
;;; What do we do with this?
class TypedTuple(traitlets.Container):
    """A trait for a tuple of any length with type-checked elements."""
    klass = tuple
    _cast_types = (list,)


def bytes_from_json(js, obj):
    return None if js is None else js.tobytes()

bytes_serialization = {
    'from_json': bytes_from_json,
}
||#
