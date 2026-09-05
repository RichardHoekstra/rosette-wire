;;;; core.lisp --- Chemical formula parsing and molecular weights.

(in-package #:rosette-chemical-formula)

(defstruct (chemical-element
            (:constructor %make-chemical-element
                (atomic-number symbol name atomic-weight mass-basis)))
  atomic-number
  symbol
  name
  atomic-weight
  mass-basis)

(defparameter +periodic-table+
  (list
   (%make-chemical-element 1 "H" "hydrogen" 1.00794d0 :standard)
   (%make-chemical-element 2 "He" "helium" 4.002602d0 :standard)
   (%make-chemical-element 3 "Li" "lithium" 6.941d0 :standard)
   (%make-chemical-element 4 "Be" "beryllium" 9.012182d0 :standard)
   (%make-chemical-element 5 "B" "boron" 10.811d0 :standard)
   (%make-chemical-element 6 "C" "carbon" 12.0107d0 :standard)
   (%make-chemical-element 7 "N" "nitrogen" 14.0067d0 :standard)
   (%make-chemical-element 8 "O" "oxygen" 15.9994d0 :standard)
   (%make-chemical-element 9 "F" "fluorine" 18.9984032d0 :standard)
   (%make-chemical-element 10 "Ne" "neon" 20.1797d0 :standard)
   (%make-chemical-element 11 "Na" "sodium" 22.98976928d0 :standard)
   (%make-chemical-element 12 "Mg" "magnesium" 24.305d0 :standard)
   (%make-chemical-element 13 "Al" "aluminium" 26.9815386d0 :standard)
   (%make-chemical-element 14 "Si" "silicon" 28.0855d0 :standard)
   (%make-chemical-element 15 "P" "phosphorus" 30.973762d0 :standard)
   (%make-chemical-element 16 "S" "sulfur" 32.065d0 :standard)
   (%make-chemical-element 17 "Cl" "chlorine" 35.453d0 :standard)
   (%make-chemical-element 18 "Ar" "argon" 39.948d0 :standard)
   (%make-chemical-element 19 "K" "potassium" 39.0983d0 :standard)
   (%make-chemical-element 20 "Ca" "calcium" 40.078d0 :standard)
   (%make-chemical-element 21 "Sc" "scandium" 44.955912d0 :standard)
   (%make-chemical-element 22 "Ti" "titanium" 47.867d0 :standard)
   (%make-chemical-element 23 "V" "vanadium" 50.9415d0 :standard)
   (%make-chemical-element 24 "Cr" "chromium" 51.9961d0 :standard)
   (%make-chemical-element 25 "Mn" "manganese" 54.938045d0 :standard)
   (%make-chemical-element 26 "Fe" "iron" 55.845d0 :standard)
   (%make-chemical-element 27 "Co" "cobalt" 58.933195d0 :standard)
   (%make-chemical-element 28 "Ni" "nickel" 58.6934d0 :standard)
   (%make-chemical-element 29 "Cu" "copper" 63.546d0 :standard)
   (%make-chemical-element 30 "Zn" "zinc" 65.38d0 :standard)
   (%make-chemical-element 31 "Ga" "gallium" 69.723d0 :standard)
   (%make-chemical-element 32 "Ge" "germanium" 72.64d0 :standard)
   (%make-chemical-element 33 "As" "arsenic" 74.9216d0 :standard)
   (%make-chemical-element 34 "Se" "selenium" 78.96d0 :standard)
   (%make-chemical-element 35 "Br" "bromine" 79.904d0 :standard)
   (%make-chemical-element 36 "Kr" "krypton" 83.798d0 :standard)
   (%make-chemical-element 37 "Rb" "rubidium" 85.4678d0 :standard)
   (%make-chemical-element 38 "Sr" "strontium" 87.62d0 :standard)
   (%make-chemical-element 39 "Y" "yttrium" 88.90585d0 :standard)
   (%make-chemical-element 40 "Zr" "zirconium" 91.224d0 :standard)
   (%make-chemical-element 41 "Nb" "niobium" 92.90638d0 :standard)
   (%make-chemical-element 42 "Mo" "molybdenum" 95.96d0 :standard)
   (%make-chemical-element 43 "Tc" "technetium" 98d0 :representative)
   (%make-chemical-element 44 "Ru" "ruthenium" 101.07d0 :standard)
   (%make-chemical-element 45 "Rh" "rhodium" 102.9055d0 :standard)
   (%make-chemical-element 46 "Pd" "palladium" 106.42d0 :standard)
   (%make-chemical-element 47 "Ag" "silver" 107.8682d0 :standard)
   (%make-chemical-element 48 "Cd" "cadmium" 112.411d0 :standard)
   (%make-chemical-element 49 "In" "indium" 114.818d0 :standard)
   (%make-chemical-element 50 "Sn" "tin" 118.71d0 :standard)
   (%make-chemical-element 51 "Sb" "antimony" 121.76d0 :standard)
   (%make-chemical-element 52 "Te" "tellurium" 127.6d0 :standard)
   (%make-chemical-element 53 "I" "iodine" 126.90447d0 :standard)
   (%make-chemical-element 54 "Xe" "xenon" 131.293d0 :standard)
   (%make-chemical-element 55 "Cs" "caesium" 132.9054519d0 :standard)
   (%make-chemical-element 56 "Ba" "barium" 137.327d0 :standard)
   (%make-chemical-element 57 "La" "lanthanum" 138.90547d0 :standard)
   (%make-chemical-element 58 "Ce" "cerium" 140.116d0 :standard)
   (%make-chemical-element 59 "Pr" "praseodymium" 140.90765d0 :standard)
   (%make-chemical-element 60 "Nd" "neodymium" 144.242d0 :standard)
   (%make-chemical-element 61 "Pm" "promethium" 145d0 :representative)
   (%make-chemical-element 62 "Sm" "samarium" 150.36d0 :standard)
   (%make-chemical-element 63 "Eu" "europium" 151.964d0 :standard)
   (%make-chemical-element 64 "Gd" "gadolinium" 157.25d0 :standard)
   (%make-chemical-element 65 "Tb" "terbium" 158.92535d0 :standard)
   (%make-chemical-element 66 "Dy" "dysprosium" 162.5d0 :standard)
   (%make-chemical-element 67 "Ho" "holmium" 164.93032d0 :standard)
   (%make-chemical-element 68 "Er" "erbium" 167.259d0 :standard)
   (%make-chemical-element 69 "Tm" "thulium" 168.93421d0 :standard)
   (%make-chemical-element 70 "Yb" "ytterbium" 173.054d0 :standard)
   (%make-chemical-element 71 "Lu" "lutetium" 174.9668d0 :standard)
   (%make-chemical-element 72 "Hf" "hafnium" 178.49d0 :standard)
   (%make-chemical-element 73 "Ta" "tantalum" 180.94788d0 :standard)
   (%make-chemical-element 74 "W" "tungsten" 183.84d0 :standard)
   (%make-chemical-element 75 "Re" "rhenium" 186.207d0 :standard)
   (%make-chemical-element 76 "Os" "osmium" 190.23d0 :standard)
   (%make-chemical-element 77 "Ir" "iridium" 192.217d0 :standard)
   (%make-chemical-element 78 "Pt" "platinum" 195.084d0 :standard)
   (%make-chemical-element 79 "Au" "gold" 196.966569d0 :standard)
   (%make-chemical-element 80 "Hg" "mercury" 200.59d0 :standard)
   (%make-chemical-element 81 "Tl" "thallium" 204.3833d0 :standard)
   (%make-chemical-element 82 "Pb" "lead" 207.2d0 :standard)
   (%make-chemical-element 83 "Bi" "bismuth" 208.9804d0 :standard)
   (%make-chemical-element 84 "Po" "polonium" 209d0 :representative)
   (%make-chemical-element 85 "At" "astatine" 210d0 :representative)
   (%make-chemical-element 86 "Rn" "radon" 222d0 :representative)
   (%make-chemical-element 87 "Fr" "francium" 223d0 :representative)
   (%make-chemical-element 88 "Ra" "radium" 226d0 :representative)
   (%make-chemical-element 89 "Ac" "actinium" 227d0 :representative)
   (%make-chemical-element 90 "Th" "thorium" 232.03806d0 :standard)
   (%make-chemical-element 91 "Pa" "protactinium" 231.03588d0 :standard)
   (%make-chemical-element 92 "U" "uranium" 238.02891d0 :standard)
   (%make-chemical-element 93 "Np" "neptunium" 237d0 :representative)
   (%make-chemical-element 94 "Pu" "plutonium" 244d0 :representative)
   (%make-chemical-element 95 "Am" "americium" 243d0 :representative)
   (%make-chemical-element 96 "Cm" "curium" 247d0 :representative)
   (%make-chemical-element 97 "Bk" "berkelium" 247d0 :representative)
   (%make-chemical-element 98 "Cf" "californium" 251d0 :representative)
   (%make-chemical-element 99 "Es" "einsteinium" 252d0 :representative)
   (%make-chemical-element 100 "Fm" "fermium" 257d0 :representative)
   (%make-chemical-element 101 "Md" "mendelevium" 258d0 :representative)
   (%make-chemical-element 102 "No" "nobelium" 259d0 :representative)
   (%make-chemical-element 103 "Lr" "lawrencium" 262d0 :representative)
   (%make-chemical-element 104 "Rf" "rutherfordium" 267d0 :representative)
   (%make-chemical-element 105 "Db" "dubnium" 270d0 :representative)
   (%make-chemical-element 106 "Sg" "seaborgium" 271d0 :representative)
   (%make-chemical-element 107 "Bh" "bohrium" 270d0 :representative)
   (%make-chemical-element 108 "Hs" "hassium" 277d0 :representative)
   (%make-chemical-element 109 "Mt" "meitnerium" 278d0 :representative)
   (%make-chemical-element 110 "Ds" "darmstadtium" 281d0 :representative)
   (%make-chemical-element 111 "Rg" "roentgenium" 282d0 :representative)
   (%make-chemical-element 112 "Cn" "copernicium" 285d0 :representative)
   (%make-chemical-element 113 "Nh" "nihonium" 286d0 :representative)
   (%make-chemical-element 114 "Fl" "flerovium" 289d0 :representative)
   (%make-chemical-element 115 "Mc" "moscovium" 290d0 :representative)
   (%make-chemical-element 116 "Lv" "livermorium" 293d0 :representative)
   (%make-chemical-element 117 "Ts" "tennessine" 294d0 :representative)
   (%make-chemical-element 118 "Og" "oganesson" 294d0 :representative))
  "Periodic-table records for H through Og.
Atomic weights are kg/kmol; radioactive/synthetic entries marked
:REPRESENTATIVE use engineering fallback mass numbers.")

(defparameter +atomic-weights+
  (mapcar (lambda (element)
            (cons (chemical-element-symbol element)
                  (chemical-element-atomic-weight element)))
          +periodic-table+)
  "Atomic weights for H through Og in kg/kmol, derived from +PERIODIC-TABLE+.")

(defun %string-equal (a b)
  (string= (string-upcase a) (string-upcase b)))

(defun find-element (designator &key (errorp t))
  "Return the periodic-table record for DESIGNATOR.
DESIGNATOR may be an atomic number, symbol, name, or CHEMICAL-ELEMENT."
  (let ((element
          (cond
            ((chemical-element-p designator) designator)
            ((integerp designator)
             (find designator +periodic-table+
                   :key #'chemical-element-atomic-number))
            ((stringp designator)
             (find designator +periodic-table+
                   :test #'%string-equal
                   :key (lambda (element)
                          (chemical-element-symbol element)))
             )
            ((symbolp designator)
             (find-element (symbol-name designator) :errorp nil)))))
    (when (and (null element) (stringp designator))
      (setf element
            (find designator +periodic-table+
                  :test #'%string-equal
                  :key #'chemical-element-name)))
    (cond
      (element element)
      (errorp (error "No periodic-table element for ~S." designator))
      (t nil))))

(defun element-symbols ()
  "Return the ordered list of element symbols H through Og."
  (mapcar #'chemical-element-symbol +periodic-table+))

(defun atomic-weight (designator)
  "Return the atomic weight for DESIGNATOR in kg/kmol."
  (chemical-element-atomic-weight (find-element designator)))

(defun %digit-value (char)
  (digit-char-p char 10))

(defun %read-integer-at (string start)
  (let ((i start)
        (value 0)
        (saw nil))
    (loop while (and (< i (length string))
                     (%digit-value (char string i)))
          do (setf saw t
                   value (+ (* value 10) (%digit-value (char string i))))
             (incf i))
    (values (if saw value 1) i)))

(defun parse-formula (formula)
  "Parse a simple chemical formula into an element-count alist.
Supports element symbols with optional integer counts, e.g. \"CH3OH\",
\"CO2\", and \"CaCO3\". Parentheses and hydrates are intentionally out of
scope for this primitive."
  (check-type formula string)
  (let ((i 0)
        (acc nil))
    (loop while (< i (length formula)) do
      (let ((char (char formula i)))
        (unless (upper-case-p char)
          (error "Expected uppercase element symbol at ~D in ~S." i formula))
        (let ((start i))
          (incf i)
          (when (and (< i (length formula))
                     (lower-case-p (char formula i)))
            (incf i))
          (multiple-value-bind (count next)
              (%read-integer-at formula i)
            (let* ((element (subseq formula start i))
                   (slot (assoc element acc :test #'string=)))
              (unless (assoc element +atomic-weights+ :test #'string=)
                (error "No atomic weight for element ~S." element))
              (if slot
                  (incf (cdr slot) count)
                  (push (cons element count) acc)))
            (setf i next)))))
    (sort acc #'string< :key #'car)))

(defun formula-molecular-weight (formula &optional (weights +atomic-weights+))
  "Return molecular weight for FORMULA in kg/kmol.
FORMULA may be a string or an alist from PARSE-FORMULA."
  (let ((alist (if (stringp formula) (parse-formula formula) formula)))
    (reduce #'+ alist
            :key (lambda (pair)
                   (let ((weight (cdr (assoc (car pair) weights
                                             :test #'string=))))
                     (unless weight
                       (error "No atomic weight for element ~S." (car pair)))
                     (* (cdr pair) weight)))
            :initial-value 0d0)))
