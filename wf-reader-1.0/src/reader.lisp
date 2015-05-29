(in-package :cl-user)

(defpackage :z.silver7017.windforge-reader
  (:use :common-lisp))

(in-package :z.silver7017.windforge-reader)

;;;; The following was taken from gigamonkeys.com/book, see license file

;;;macro helper stuff
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (string n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defun spliceable (value)
  (if value (list value)))

(defmacro ppme (form &environment env)
  (progn
    (write (macroexpand-1 form env)
           :length nil
           :level nil
           :circle nil
           :pretty t
           :gensym nil
           :right-margin 83
           :case :downcase)
    nil))

;;;file reader helper stuff
(defun list-directory (dirname)
  "Return a list of the contents of the directory named by dirname.
Names of subdirectories will be returned in `directory normal
form'. Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept
wildcard pathnames; `dirname' should simply be a pathname that
names a directory. It can be in either file or directory form."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))

  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
    ;; form just the way we want.
    (directory wildcard)
    
    #+openmcl
    ;; OpenMCl by default doesn't return subdirectories at all. But
    ;; when prodded to do so with the special argument :directories,
    ;; it returns them in directory form.
    (directory wildcard :directories t)
            
    #+allegro
    ;; Allegro normally return directories in file form but we can
    ;; change that with the :directories-are-files argument.
    (directory wildcard :directories-are-files nil)
            
    #+clisp
    ;; CLISP has a particularly idiosyncratic view of things. But we
    ;; can bludgeon even it into doing what we want.
    (nconc 
     ;; CLISP won't list files without an extension when :type is
     ;; wild so we make a special wildcard for it.
     (directory wildcard)
     ;; And CLISP doesn't consider subdirectories to match unless
     ;; there is a :wild in the directory component.
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))




(defun file-exists-p (pathname)
  "Similar to CL:PROBE-FILE except it always returns directory names
in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."

  #+(or sbcl lispworks openmcl)
  ;; These implementations do "The Right Thing" as far as we are
  ;; concerned. They return a truename of the file or directory if it
  ;; exists and the truename of a directory is in directory normal
  ;; form.
  (probe-file pathname)

  #+(or allegro cmu)
  ;; These implementations accept the name of a directory in either
  ;; form and return the name in the form given. However the name of a
  ;; file must be given in file form. So we try first with a directory
  ;; name which will return NIL if either the file doesn't exist at
  ;; all or exists and is not a directory. Then we try with a file
  ;; form name.
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  ;; Once again CLISP takes a particularly unforgiving approach,
  ;; signalling ERRORs at the slightest provocation.

  ;; pathname in file form and actually a file      -- (probe-file file)      ==> truename
  ;; pathname in file form and doesn't exist        -- (probe-file file)      ==> NIL
  ;; pathname in dir form and actually a directory  -- (probe-directory file) ==> truename
  ;; pathname in dir form and doesn't exist         -- (probe-directory file) ==> NIL

  ;; pathname in file form and actually a directory -- (probe-file file)      ==> ERROR
  ;; pathname in dir form and actually a file       -- (probe-directory file) ==> ERROR
  (or (ignore-errors
        ;; PROBE-FILE will return the truename if file exists and is a
        ;; file or NIL if it doesn't exist at all. If it exists but is
        ;; a directory PROBE-FILE will signal an error which we
        ;; ignore.
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        ;; PROBE-DIRECTORY returns T if the file exists and is a
        ;; directory or NIL if it doesn't exist at all. If it exists
        ;; but is a file, PROBE-DIRECTORY will signal an error.
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))


    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))

(defun directory-wildcard (dirname)
  (make-pathname 
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))


(defun directory-pathname-p (p)
  "Is the given pathname the name of a directory? This function can
usefully be used to test whether a name returned by LIST-DIRECTORIES
or passed to the function in WALK-DIRECTORY is the name of a directory
in the file system since they always return names in `directory normal
form'."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))


(defun file-pathname-p (p)
  (unless (directory-pathname-p p) p))

(defun pathname-as-directory (name)
  "Return a pathname reperesenting the given pathname in
`directory normal form', i.e. with all the name elements in the
directory component and NIL in the name and type components. Can
not be used on wild pathnames because there's not portable way to
convert wildcards in the name and type into a single directory
component. Returns its argument if name and type are both nil or
:unspecific."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname 
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))

(defun pathname-as-file (name)
  "Return a pathname reperesenting the given pathname in `file form',
i.e. with the name elements in the name and type component. Can't
convert wild pathnames because of problems mapping wild directory
component into name and type components. Returns its argument if
it is already in file form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname 
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "Walk a directory invoking `fn' on each pathname found. If `test' is
supplied fn is invoked only on pathnames for which `test' returns
true. If `directories' is t invokes `test' and `fn' on directory
pathnames as well."
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

(defun directory-p (name)
  "Is `name' the name of an existing directory."
  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))))

(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))))


;;;binary file stuff
(defvar *in-progress-objects* nil)

(defconstant +null+ (code-char 0))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))


;;; Binary types

(defmacro define-binary-type (name (&rest args) &body spec)
  (with-gensyms (type stream value)
  `(progn
    (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
      (declare (ignorable ,@args))
      ,(type-reader-body spec stream))
    (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
      (declare (ignorable ,@args))
      ,(type-writer-body spec stream value)))))

(defun type-reader-body (spec stream)
  (ecase (length spec)
    (1 (destructuring-bind (type &rest args) (mklist (first spec))
         `(read-value ',type ,stream ,@args)))
    (2 (destructuring-bind ((in) &body body) (cdr (assoc :reader spec))
         `(let ((,in ,stream)) ,@body)))))

(defun type-writer-body (spec stream value)
  (ecase (length spec)
    (1 (destructuring-bind (type &rest args) (mklist (first spec))
         `(write-value ',type ,stream ,value ,@args)))
    (2 (destructuring-bind ((out v) &body body) (cdr (assoc :writer spec))
         `(let ((,out ,stream) (,v ,value)) ,@body)))))


;;; Binary classes

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name 'slots) ',(mapcar #'first slots))
         (setf (get ',name 'superclasses) ',superclasses))
       
       (defclass ,name ,superclasses
         ,(mapcar #'slot->defclass-slot slots))
       
       ,read-method
       
       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
      (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
        (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
          (let ((,objectvar
                 (make-instance 
                  ,@(or (cdr (assoc :dispatch options))
                        (error "Must supply :disptach form."))
                  ,@(mapcan #'slot->keyword-arg slots))))
            (read-object ,objectvar ,streamvar)
            ,objectvar))))))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x) (if (listp x) x (list x)))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

;;; Keeping track of inherited slots

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  "Like all slots but works while compiling a new class before slots
and superclasses have been saved."
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

;;; In progress Object stack

(defun current-binary-object ()
  (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A few basic types

;;; Unsigned Integers

(define-binary-type unsigned-integer (bytes)
  (:reader (in)
    (loop with value = 0
       for low-bit downfrom (* 8 (1- bytes)) to 0 by 8 do
         (setf (ldb (byte 8 low-bit) value) (read-byte in))
       finally (return value)))
  (:writer (out value)
    (loop for low-bit downfrom (* 8 (1- bytes)) to 0 by 8
       do (write-byte (ldb (byte 8 low-bit) value) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1))
(define-binary-type u2 () (unsigned-integer :bytes 2))
(define-binary-type u3 () (unsigned-integer :bytes 3))
(define-binary-type u4 () (unsigned-integer :bytes 4))

;;; Strings
(define-binary-type generic-string (length character-type)
  (:reader (in)
    (let ((string (make-string length)))
      (dotimes (i length)
        (setf (char string i) (read-value character-type in)))
      string))
  (:writer (out string)
    (dotimes (i length)
      (write-value character-type out (char string i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
    (with-output-to-string (s)
      (loop for char = (read-value character-type in)
            until (char= char terminator) do (write-char char s))))
  (:writer (out string)
    (loop for char across string
          do (write-value character-type out char)
          finally (write-value character-type out terminator))))

;;; UCS-2 (Unicode) strings (i.e. UTF-16 without surrogate pairs, phew.)

;;; Define a binary type for reading a UCS-2 character relative to a
;;; particular byte ordering as indicated by the BOM value.
 ;; v2.3 specifies that the BOM should be present. v2.2 is silent
 ;; though it is arguably inherent in the definition of UCS-2) Length
 ;; is in bytes. On the write side, since we don't have any way of
 ;; knowing what BOM was used to read the string we just pick one.
 ;; This does mean roundtrip transparency could be broken.

(define-binary-type ucs-2-char (swap)
  (:reader (in)
    (let ((code (read-value 'u2 in)))
      (when swap (setf code (swap-bytes code)))
      (or (code-char code) (error "Character code ~d not supported" code))))
  (:writer (out char)
    (let ((code (char-code char)))
      (unless (<= 0 code #xffff)
        (error "Illegal character for ucs-2 encoding: ~c with char-code: ~d" char code))
      (when swap (setf code (swap-bytes code)))
      (write-value 'u2 out code))))

(defun swap-bytes (code)
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
  code)

(define-binary-type ucs-2-char-big-endian () (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-little-endian () (ucs-2-char :swap t))

(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (#xfeff 'ucs-2-char-big-endian)
    (#xfffe 'ucs-2-char-little-endian)))

(define-binary-type raw-bytes (size)
  (:reader (in)
    (let ((buf (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf))
  (:writer (out buf)
    (write-sequence buf out)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; silver7017 - my code starts here

;;; this is written in CLISP, might not be compatible with other flavors


;; read and writes windforge data file header style integers and strings
(define-binary-type wf-header-size (bytes)
  (:reader (in)
    (loop with value = 0
       for low-bit from 0 to (* 8 (1- bytes)) by 8 do
         (setf (ldb (byte 8 low-bit) value) (read-byte in))
       finally (return value)))
  (:writer (out value)
    (loop for low-bit from 0 to (* 8 (1- bytes)) by 8
       do (write-byte (ldb (byte 8 low-bit) value) out))))

(define-binary-type wf-string ()
  (:reader (in)
    (read-value
     'generic-terminated-string in
     :terminator #\linefeed
     :character-type (ucs-2-char-type #xfffe)))
  (:writer (out string)
    (write-value 
     'generic-terminated-string out string
     :terminator #\linefeed
     :character-type (ucs-2-char-type #xfffe))))

;reads an actual header value using the primitives defined above
(define-binary-type wf-file-info ()
  (:reader (in)
    (list
      (parse-integer (read-value ;size
       'generic-terminated-string in
       :terminator #\space
       :character-type (ucs-2-char-type #xfffe)))
      (parse-integer (read-value ;position
       'generic-terminated-string in
       :terminator #\space
       :character-type (ucs-2-char-type #xfffe)))
      (callout (read-value       ;filepath
       'generic-terminated-string in
       :terminator #\linefeed
       :character-type (ucs-2-char-type #xfffe)) T )))
  (:writer (out strings)
    (write-value ;size
     'generic-terminated-string out (write-to-string (first strings))
     :terminator #\space
     :character-type (ucs-2-char-type #xfffe))
    (write-value ;position
     'generic-terminated-string out (write-to-string (second strings))
     :terminator #\space
     :character-type (ucs-2-char-type #xfffe))
    (write-value ;filepath
     'generic-terminated-string out (third strings)
     :terminator #\linefeed
     :character-type (ucs-2-char-type #xfffe))))

(defparameter *header-size* 4) ;number of bytes for the integer at the start of the header

(define-binary-type wf-header (size) ;this reads or writes the collection of all file headers in the data file
  (:reader (in)
    (loop for header = (read-value 'wf-file-info in) then (read-value 'wf-file-info in)
          until (= size (file-position in))
          collect header))
  (:writer (out header)
    (loop for item in header
          do (write-value 'wf-file-info out item))))

(define-binary-class wf-data () ;holds the entire header
  ((size    (wf-header-size :bytes *header-size*))
   (header  (wf-header :size (+ size *header-size*)))))

(defun read-wf-data (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'wf-data in)))

(defparameter *wf-default* "C:\\Program Files\\Steam\\SteamApps\\common\\Windforge\\Data\\BIG.FILE")

(defun set-dir (dir) (setf *wf-default* (namestring dir))) ;change the default directory

(defparameter *wf-custom* "C:\\things\\install\\Steam\\SteamApps\\common\\Windforge\\Data\\BIG.FILE") ;my default directory.

(defvar *wf-data* nil) ;store the wf-data object

(defun load-wf-data ((file *wf-default*)) ;this loads the header into memory so that it can be manipulated
  (setf *wf-data* (read-wf-data file)))

(defun callout (val &optional (nl nil) (report nil)) ;so you don't sit at a blank screen wondering if anything is happening
  (format t "~a" val)
  (if nl (format t "~%"))
  (if report (format t "~a~%" report))
  (return-from callout val))

;;;the following relates to actually extracting files

;helper function
(defun stream-copy (in out size)
  (if (<= size array-total-size-limit)
    (let ((buf (make-array size :element-type '(unsigned-byte 8))))
      (ext:read-byte-sequence buf in :end size)
      (ext:write-byte-sequence buf out))
    (error "Block too large to copy")))
  

;this extracts all of the files from the data file, but only after the file has been read
(defun extract-all (&optional (file *wf-default*) (dir (directory-namestring file)))
  (if (not *wf-data*) (load-wf-data file))
  (ext:cd dir)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (loop for item in (header *wf-data*) do
      (let ((file-size (first item)) (file-pos (second item)) (file-name (third item)))
        (format t "~a~%" file-name) ;print out what file we are extracting
        (file-position in (+ file-pos (size *wf-data*) *header-size*)) ;move the positions in the stream to the start of the file we are extracting
        (with-open-file (out (ensure-directories-exist file-name) :element-type '(unsigned-byte 8) :direction :output)
          (if (<= file-size array-total-size-limit)
            (stream-copy in out file-size) ; if the file fits in one buffer
            (multiple-value-bind (num remain) (floor file-size array-total-size-limit) ;needs multiple buffers to transfer
              (loop repeat num do (stream-copy in out array-total-size-limit)
                    finally (stream-copy in out remain)))))))))

(defun main ()
  (format t "Windforge Extractor 1.0~%")
  (format t "If Steam is installed in the default directory, type \(extract-all\)~%")
  (format t "to extract the data files into your windforge directory.~%For advanced options, see the manual.~%"))
