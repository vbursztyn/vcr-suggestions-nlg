;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: vcr-question-form.lsp
;;;;    System: CogSketch
;;;;    Author: Emmett Tomai
;;;;   Created: November 5, 2002 13:36:04
;;;;   Purpose: Dialog for user clarification of visual/conceptual relations
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2016-08-01 18:25:07 -0500 (Mon, 01 Aug 2016) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :cogsketch)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Questions Frameset

(rbrowse:publish :path (rbrowse:make-relative-url "vcr-questions-frameset.html")
                 :content-type "text/html"
                 :function 'gen-vcr-questions-frameset-page)

(defun vcr-questions-frameset-url (rbrowse reasoner sketch)
  (let ((rbrowse-item (rbrowse:make-item rbrowse reasoner sketch)))
    (rbrowse:make-relative-url "vcr-questions-frameset.html"
      :keys (list (cons "id" (rbrowse:id rbrowse-item))))))

(defun gen-vcr-questions-frameset-page (rbrowse)
  (multiple-value-bind (rbrowse-item id)
      (rbrowse:get-item-from-url-keys rbrowse)
    (if rbrowse-item
      (make-vcr-questions-frameset-page rbrowse (rbrowse:data-source rbrowse-item)
                                        (rbrowse:datum rbrowse-item))
      (rbrowse:with-rbrowse-page (rbrowse)
        (rbrowse:describe-error rbrowse nil
          (format nil "Could not find any reasoners whose ID is ~A." id))))))

(defun make-vcr-questions-frameset-page (rbrowse reasoner sketch)
  (rbrowse:with-rbrowse-page
      (rbrowse 
       :title (format nil "visual/conceptual relationships (~A)"
                (user-namestring sketch))
       :frames? t)
    (net.html.generator:html 
     ((:frameset cols "200, *") :newline
      ((:frameset rows #.(format nil "*, ~D" *controls-menu-height*)) :newline
       ((:frame name "objects" src (objnames-url rbrowse sketch))) :newline
       ((:frame name "controls" src (controls-url rbrowse sketch)))) :newline
      ((:frame name "main" src (vcr-questions-url rbrowse reasoner)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Questions Page Publishing and Rbrowse integration

(rbrowse:publish :path (rbrowse:make-relative-url "visual-conceptual-relations.html")
                 :content-type "text/html"
                 :function 'gen-vcr-questions-page)


(defun vcr-questions-url (rbrowse reasoner)
  (let ((rbrowse-item (rbrowse:make-item rbrowse reasoner nil)))
    (rbrowse:make-relative-url "visual-conceptual-relations.html"
      :keys (list (cons "id" (rbrowse:id rbrowse-item))))))


(defun gen-vcr-questions-page (rbrowse)
  (multiple-value-bind (rbrowse-item id)
      (rbrowse:get-item-from-url-keys rbrowse)
    (if rbrowse-item
      (make-vcr-questions-page rbrowse (rbrowse:data-source rbrowse-item))
      (rbrowse:with-rbrowse-page (rbrowse)
        (rbrowse:describe-error rbrowse nil
          (format nil "Could not find any reasoners whose ID is ~A." id))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Questions Page Display

(defun make-vcr-questions-page (rbrowse reasoner)
  (let ((sketch (sketch reasoner))
        (responses (get-vcr-responses rbrowse)))
    (unless responses
      (publish-event (core sketch) :vcr-question-answering-started
        :sketch sketch))
    (handle-vcr-responses responses reasoner)
    (rbrowse:with-rbrowse-page (rbrowse)
      (net.html.generator:html
       (vcr-questions-header sketch)
       ((:div :id "basic-content") :newline
        (dolist (subsketch (subsketches sketch))
          (let ((subsketch-case (case-name subsketch))
                (subsketch-namestr (user-namestring subsketch)))
            (net.html.generator:html
             :newline
             (:h3 (:princ "Subsketch " subsketch-namestr ":")) 
             :newline
             (let ((questions (gather-visual-conceptual-relation-questions
                               subsketch-case reasoner)))
               (cond ((null questions)
                      (net.html.generator:html (:i "No questions currently.")
                                               (:br) (:newline)))
                     (t
                      (net.html.generator:html
                       ((:form name (format nil "form~A" subsketch-case) 
                               action (vcr-questions-url rbrowse reasoner) 
                               method "POST")
                        ((:input type "hidden" name "bcase" value subsketch-case))
                        (display-vcr-questions 
                         (sort-questions-by-entity-names 
                          questions subsketch-case reasoner)
                         rbrowse reasoner subsketch-case)
                        ((:input type "submit" value "Submit")))))))))))
       (vcr-questions-page-footer)))))


(defun vcr-questions-header (sketch)
  (net.html.generator:html
   ((:div class "dkpurplebox") 
    :newline
    ((:div :class "topnav-left") :newline
     ((:h3 class "sm-margins") "Visual/Conceptual Relations")
     :newline)
    :newline
    ((:div :class "topnav-right") :newline
     "sketch: " (:princ-safe (user-namestring sketch)))
    :newline
    ((:div :class "clear"))
    :newline)
   :newline))


(defun vcr-questions-page-footer ()
  nil)


;; Recall questions is a list of entries of the form
;; (<entity pair>
;;   <list of suggested relations to ask about>
;;   <list of suggested relations that happen to be true already>
;;   <list of (analogical suggestions . <user acceptance value>)>
;;   <user accepted non-analogical suggested relation>)
;;
;; the three lists of suggested relations and the single user
;; accepted non-analogical relation are mutually exclusive
;;
;; all relation suggestions are of the form: ((reln o1 o2) . <explanation>)
;;
;; analogy suggestions are consed with a believed truth value (sugg . believed?)
;;
;; the interface restircts the user to accepting only one non-
;; analogical suggestion at a time, so that is guarenteed to be
;; one or nil
;;
;; the user accepted suggestion is of the form (reln o1 o2)
;;
(defun sort-questions-by-entity-names (questions subsketch-case reasoner)
  (sort
   (mapcar
    #'(lambda (elt)
        (cons
         (let ((names 
                (list
                 (cons (caar elt)
                       (fire:get-namestring reasoner (caar elt) subsketch-case))
                 (cons (cadar elt)
                       (fire:get-namestring reasoner (cadar elt) subsketch-case)))))
           (sort names 'string-lessp :key 'cdr))
         (cdr elt)))
    questions)
   #'(lambda (names1 names2)
       (or (rbrowse:exp< (cdar names1) (cdar names2))
           (and (string= (format nil "~A" (cdar names1))
                         (format nil "~A" (cdar names2)))
                (rbrowse:exp< (cdadr names1) (cdadr names2)))))
   :key 'car))



(defun display-vcr-questions (questions rbrowse reasoner subsketch-case)
  (let ((q-idx -1))
    (dolist (question questions)
      (destructuring-bind (names possible-relns known-relns analogy-relns
                                 user-answer)
          question
        
        (setq possible-relns (filter-symmetric-relns possible-relns))

        (print-vcr-header names rbrowse reasoner subsketch-case)
        (print-vcr-question 
         rbrowse reasoner names 
         possible-relns known-relns analogy-relns user-answer (incf q-idx))))))


(defun filter-symmetric-relns (possible-relns)
  (remove-duplicates
   possible-relns
   :test #'(lambda (x y)
             (let ((exp1 (first x))
                   (exp2 (first y)))
               ;; expn = (relation object1 object 2)
               (and (equal (first exp1) (first exp2))
                    (equal (second exp1) (third exp2))
                    (equal (second exp2) (third exp1))
                    (kb:instance-of? (first exp1)
                                     'd::SymmetricRelation
                                     'd::EverythingPSC))))))


(defun print-vcr-header (names rbrowse reasoner subsketch-case)
  (let* ((sketch (sketch reasoner))
         (subsketch (and sketch (casename->subsketch subsketch-case sketch)))
         (glyph1 (and sketch (find-sketch-item (car (first names)) subsketch)))
         (glyph2 (and sketch (find-sketch-item (car (second names)) subsketch))))
    (net.html.generator:html
     ((:p)
      (:princ "Conceptual relationships between ")
      (if glyph1
        (net.html.generator:html
         ((:a href (rbrowse:wm-facts-about-objs-url rbrowse reasoner (list glyph1)))
          (:princ-safe (cdr (first names)))))
        (net.html.generator:html
         ((:span class "bold") (:princ-safe (cdr (first names))))))
      " and "
      (if glyph2
        (net.html.generator:html
         ((:a href (rbrowse:wm-facts-about-objs-url rbrowse reasoner (list glyph2)))
          (:princ-safe (cdr (second names)))))
        (net.html.generator:html
         ((:span class "bold") (:princ-safe (cdr (second names))))))
      (:princ ":"))
     :newline)))

(defun print-vcr-question (rbrowse reasoner names possible-relns known-relns 
                                   analogy-relns user-answer q-idx)
  (net.html.generator:html
   ((:div class "bluebox") :newline
    (print-vcr-user-answer names possible-relns user-answer q-idx)
    (print-vcr-known-relns rbrowse reasoner names known-relns)
    (print-vcr-analogy-relns rbrowse reasoner names analogy-relns q-idx))
   :newline))

(defun print-vcr-known-relns (rbrowse reasoner names relns)
  (when relns
    (net.html.generator:html
     :br :newline
     ((:div class "dkbluebox bold") "Additional known relationships")
     :br :br :newline
     ((:table) :newline
      (dolist (reln relns)
        (net.html.generator:html
         ((:tr) :newline
          ((:td class "no-border")
           (print-vcr-justification-link rbrowse reasoner (third reln)))
          :newline
          ((:td class "no-border")
           (print-vcr-assumptions-link rbrowse reasoner (third reln)))
          :newline
          ((:td class "no-border")
           ((:span class "fact") 
            (:princ-safe (sublis names (car reln)))))
          :newline)
         :newline))
      :newline)
     :newline)))

(defun print-vcr-analogy-relns (rbrowse reasoner names relns q-idx)
  (when relns
    (let ((as-idx -1))
      (net.html.generator:html
       :br :newline
       ((:div class "dkbluebox bold") "Relationships suggested by analogy")
       :newline
       ((:table) :newline
        (dolist (reln relns)
          (let ((fact (first (car reln)))
                (explanation (third (car reln)))
                (believed? (cdr reln)))
            (net.html.generator:html
             (:tr
              ((:td class "no-border")
               (print-vcr-justification-link rbrowse reasoner explanation))
              :newline
              ((:td class "no-border")
               (print-vcr-assumptions-link rbrowse reasoner explanation))
              :newline
              ((:td class "no-border")
               (print-vcr-analogy-reln names fact q-idx (incf as-idx) believed?)))
             :newline)))
        :newline)
       :newline))))

(defun print-vcr-analogy-reln (names reln q-idx as-idx believed?)
  (net.html.generator:html
   (:newline)
   ((:span class "fact")
    ((:pre class "fact")
     (:princ (sublis names reln))
     ((:select name (format nil "~Aanalogy~A" q-idx as-idx))
      (:newline)
      (print-option (format-relation-predicate reln names "F") 
                    (not believed?) (if believed? "Reject" "--"))
      (:newline)
      (print-option (format-relation-predicate reln names "T")
                    believed? (if believed? "Accepted" "Accept")))))))

(defun format-relation-predicate (reln names &optional pre)
  (let ((direction (if (equal (cdr reln) (mapcar #'car names))
                       "N" "R")))
    (cond ((not (null pre))
           (format nil "~A~A~A" pre direction (car reln)))
          (t
           (format nil "~A~A" direction (car reln))))))

(defun print-option (val sel label)
  (if sel
      (net.html.generator:html
       ((:option value val selected t)
        (:princ label)))
    (net.html.generator:html
     ((:option value val)
      (:princ label)))))

(defun print-vcr-user-answer (names possible-relns user-answer q-idx)
  (net.html.generator:html  
   ((:div :class "dkbluebox bold")
    "User supplied relationship")
   :newline
   ((:input type "hidden" name (format nil "~Ao1" q-idx) 
            value (car (first names))))
   :newline
   ((:input type "hidden" name (format nil "~Ao2" q-idx) 
            value (car (second names)))) 
   :newline
   (if user-answer
     (print-vcr-user-answer-with-retraction names user-answer q-idx)
     (print-vcr-reln-select names possible-relns q-idx))
   :newline))

(defun print-vcr-reln-select (names relns q-idx)
  (net.html.generator:html
   ((:p)
    (:princ 
     (format nil 
         "Which of the following best describes the relationship between ~A and ~A?"
       (cdr (first names)) (cdr (second names)))))
   :newline
   ((:select name (format nil "~Areln" q-idx))
    (:newline)
    ((:option value "noreply")
     (:princ "--"))
    (dolist (reln relns)
      (let ((pretty-exp (fire:verbalize (sublis names (first reln)) 
                          :context 'd::CogSketchNLG2Mt 
                          :fallback nil)))
        (when pretty-exp
          (net.html.generator:html
           (:newline)
           ((:option value (format-relation-predicate (car reln) names))
            (:princ pretty-exp)))))))))

(defun print-vcr-user-answer-with-retraction (names user-answer q-idx)
  (net.html.generator:html
   :newline
   ((:input type "hidden" name (format nil "~Areln" q-idx) 
            value (format-relation-predicate user-answer names)))
   :newline
   ((:p)
    ((:span class "fact")
     (:princ-safe (sublis names user-answer)))
    :newline
    " &nbsp;&nbsp;&nbsp;"
    ((:input type "checkbox" name (format nil "~Aretract" q-idx)))
    "Retract"
    :newline)))

(defun print-vcr-justification-link (rbrowse reasoner fact)
  (net.html.generator:html
   ((:a href (rbrowse:browse-item-url rbrowse reasoner fact))
    ((:img :src (rbrowse:graphics-url "why.jpg") :border "0"
           :alt "justifications")))))

(defun print-vcr-assumptions-link (rbrowse reasoner fact)
  (net.html.generator:html
   ((:a href (rbrowse:assumptions-url rbrowse reasoner fact))
    ((:img :src (rbrowse:graphics-url "assumptions.jpg") :border "0"
           :alt "assumptions")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Questions Page Response Handling

;;; retrieve the responses from the POST variables, return as a list of conses
(defun get-vcr-responses (rbrowse)
  (let ((http-req (rbrowse:http-request rbrowse)))
    (when http-req
      (net.aserve:request-query http-req :uri nil :post t))))

;;; takes the user responses as a list of key-value pairs
(defun handle-vcr-responses (pairs reasoner)
  (when pairs
    (handle-visual->conceptual-questions pairs reasoner)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry Points

(defgeneric show-vcr-questions (module sketch)
  (:documentation "Uses RBrowse to display the pages that allow users to 
                   specify the Visual -> Conceptual Relations about the glyphs
                   on the sketch."))

(defmethod show-vcr-questions (module sketch)
  (declare (ignore module sketch))
  nil)

(defmethod show-vcr-questions (module (sketch sketch))
  (declare (ignore module))
  (let* ((rbrowse (rbrowse sketch))
         (reasoner (reasoner sketch))
         (url (and rbrowse 
                   (rbrowse:make-full-url rbrowse 
                       (vcr-questions-frameset-url rbrowse reasoner sketch)))))
      (when url
        (rbrowse:show-web-page url)
        url)))



(register-control-menu-item :vcr-questions
    "Clarify Glyph Relationships"
  #'(lambda (rbrowse reasoner sketch)
      (declare (ignore sketch))
      (vcr-questions-url rbrowse reasoner))
  :order 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
