
(deftemplate hair
   (slot name)
   (slot value))

(deftemplate diagnosis
   (slot treatment))

(deffacts initial-facts
   (hair (name curly) (value no))
   (hair (name thinhair) (value no))
   (hair (name hairfall) (value no))
   (hair (name smooth) (value no))
   (hair (name grayhair) (value no))
   (hair (name lightcolor) (value no))
   (diagnosis (treatment haircut))) ;; Add initial diagnosis)


(defrule smoothing-diagnosis
   ?f1 <- (hair (name curly) (value yes))
   ?f2 <- (hair (name thinhair) (value no))
   ?f3 <- (hair (name hairfall) (value no))
   ?f4 <- (hair (name smooth) (value no))
   ?f5 <- (hair (name grayhair) (value no))
   ?f6 <- (hair (name lightcolor) (value no))
   ?d <- (diagnosis (treatment ?current))
   (test (neq ?current smoothing)) ;; Avoid duplicate diagnosis
   =>
   (retract ?d) ;; Remove old diagnosis
   (assert (diagnosis (treatment smoothing))) ;; Add new diagnosis
   (printout t "You can take smoothing treatment" crlf))

(defrule thinhair-diagnosis
   ?f1 <- (hair (name curly) (value no))
   ?f2 <- (hair (name thinhair) (value yes))
   ?f3 <- (hair (name hairfall) (value no))
   ?f4 <- (hair (name smooth) (value no))
   ?f5 <- (hair (name grayhair) (value no))
   ?f6 <- (hair (name lightcolor) (value no))
   ?d <- (diagnosis (treatment ?current))
   (test (neq ?current extension)) ;; Avoid duplicate diagnosis
   =>
   (retract ?d) ;; Remove old diagnosis
   (assert (diagnosis (treatment extension))) ;; Add new diagnosis
   (printout t "You can do hair extension" crlf))

(defrule hairfall-diagnosis
   ?f1 <- (hair (name curly) (value no))
   ?f2 <- (hair (name thinhair) (value no))
   ?f3 <- (hair (name hairfall) (value yes))
   ?f4 <- (hair (name smooth) (value no))
   ?f5 <- (hair (name grayhair) (value no))
   ?f6 <- (hair (name lightcolor) (value no))
   ?d <- (diagnosis (treatment ?current))
   (test (neq ?current creambath)) ;; Avoid duplicate diagnosis
   =>
   (retract ?d) ;; Remove old diagnosis
   (assert (diagnosis (treatment creambath))) ;; Add new diagnosis
   (printout t "You can take creambath tretment" crlf))

(defrule keratin-diagnosis
   ?f1 <- (hair (name curly) (value yes))
   ?f2 <- (hair (name thinhair) (value no))
   ?f3 <- (hair (name hairfall) (value no))
   ?f4 <- (hair (name smooth) (value yes))
   ?f5 <- (hair (name grayhair) (value no))
   ?f6 <- (hair (name lightcolor) (value no))
   ?d <- (diagnosis (treatment ?current))
   (test (neq ?current keratin)) ;; Avoid duplicate diagnosis
   =>
   (retract ?d) ;; Remove old diagnosis
   (assert (diagnosis (treatment keratin))) ;; Add new diagnosis
   (printout t "You can take keratin treatment" crlf))

(defrule lightgrayhair-diagnosis
   ?f1 <- (hair (name curly) (value no))
   ?f2 <- (hair (name thinhair) (value no))
   ?f3 <- (hair (name hairfall) (value no))
   ?f4 <- (hair (name smooth) (value no))
   ?f5 <- (hair (name grayhair) (value yes))
   ?f6 <- (hair (name lightcolor) (value yes))
   ?d <- (diagnosis (treatment ?current))
   (test (neq ?current lightcolortreat)) ;; Avoid duplicate diagnosis
   =>
   (retract ?d) ;; Remove old diagnosis
   (assert (diagnosis (treatment lightcolortreat))) ;; Add new diagnosis
   (printout t "You can do bleaching first, then you can color your hair with your desired light color" crlf))

(defrule darkgrayhair-diagnosis
   ?f1 <- (hair (name curly) (value no))
   ?f2 <- (hair (name thinhair) (value no))
   ?f3 <- (hair (name hairfall) (value no))
   ?f4 <- (hair (name smooth) (value no))
   ?f5 <- (hair (name grayhair) (value yes))
   ?f6 <- (hair (name lightcolor) (value no))
   ?d <- (diagnosis (treatment ?current))
   (test (neq ?current darkcolortreat)) ;; Avoid duplicate diagnosis
   =>
   (retract ?d) ;; Remove old diagnosis
   (assert (diagnosis (treatment darkcolortreat))) ;; Add new diagnosis
   (printout t "You can immediately color your hair with your desired dark color" crlf))

;; Rule to diagnose cutting hair
(defrule cuttinghair-diagnosis
   ?f1 <- (hair (name curly) (value no))
   ?f2 <- (hair (name thinhair) (value no))
   ?f3 <- (hair (name hairfall) (value no))
   ?f4 <- (hair (name smooth) (value no))
   ?f5 <- (hair (name grayhair) (value no))
   ?f6 <- (hair (name lightcolor) (value no))
   ?d <- (diagnosis (treatment ?current))
   (test (neq ?current cuttinghair)) ;; Avoid duplicate diagnosis
   =>
   (retract ?d) ;; Remove old diagnosis
   (assert (diagnosis (treatment cuttinghair))) ;; Add new diagnosis
   (printout t "You can do hair cut" crlf))

;; Function to change the curly fact
(deffunction change-curly(?new-value)
   "Changes the value of the curly hair"
   (bind ?curly-facts (find-all-facts ((?f hair)) (eq ?f:name curly))) ;; Get all matching curly facts
   (if (neq (length$ ?curly-facts) 0) ;; Check if any fever fact exists
      then
         (do-for-all-facts ((?f hair)) ;; Loop through all matching curly facts
            (eq ?f:name curly)
            (retract ?f)) ;; Retract the old curly fact
         (assert (hair (name curly) (value ?new-value))) ;; Assert the updated curly fact
         (printout t "curly updated to: " ?new-value crlf)
      else
         (printout t "Error: Curly fact not found!" crlf)
   )
)

;; Function to change the thinhair fact
(deffunction change-thinhair(?new-value)
   "Changes the value of the thin hair"
   (bind ?thinhair-facts (find-all-facts ((?f hair)) (eq ?f:name thinhair))) ;; Get all matching thin hair facts
   (if (neq (length$ ?thinhair-facts) 0) ;; Check if any thin hair fact exists
      then
         (do-for-all-facts ((?f hair)) ;; Loop through all matching thin hair facts
            (eq ?f:name thinhair)
            (retract ?f)) ;; Retract the old thinhair fact
         (assert (hair (name thinhair) (value ?new-value))) ;; Assert the updated thin hair fact
         (printout t "thinhair updated to: " ?new-value crlf)
      else
         (printout t "Error: thinhairfact not found!" crlf)
   )
)

;; Function to change the hairfall fact
(deffunction change-hairfall(?new-value)
   "Changes the value of the hairfall"
   (bind ?hairfall-facts (find-all-facts ((?f hair)) (eq ?f:name hairfall))) ;; Get all matching hairfall facts
   (if (neq (length$ ?hairfall-facts) 0) ;; Check if any hairfall fact exists
      then
         (do-for-all-facts ((?f hair)) ;; Loop through all matching hairfall facts
            (eq ?f:name hairfall)
            (retract ?f)) ;; Retract the old hairfall fact
         (assert (hair (name hairfall) (value ?new-value))) ;; Assert the updated hairfall fact
         (printout t "hairfall updated to: " ?new-value crlf)
      else
         (printout t "Error: hairfall fact not found!" crlf)
   )
)

;; Function to change the smooth fact
(deffunction change-smooth(?new-value)
   "Changes the value of the smooth"
   (bind ?smooth-facts (find-all-facts ((?f hair)) (eq ?f:name smooth))) ;; Get all matching smooth facts
   (if (neq (length$ ?smooth-facts) 0) ;; Check if any smooth fact exists
      then
         (do-for-all-facts ((?f hair)) ;; Loop through all matching smooth facts
            (eq ?f:name smooth)
            (retract ?f)) ;; Retract the old smooth fact
         (assert (hair (name smooth) (value ?new-value))) ;; Assert the updated smooth fact
         (printout t "smooth updated to: " ?new-value crlf)
      else
         (printout t "Error: smooth fact not found!" crlf)
   )
)

;; Function to change the grayhair fact
(deffunction change-grayhair(?new-value)
   "Changes the value of the grayhair"
   (bind ?grayhair-facts (find-all-facts ((?f hair)) (eq ?f:name smooth))) ;; Get all matching grayhair facts
   (if (neq (length$ ?grayhair-facts) 0) ;; Check if any grayhair fact exists
      then
         (do-for-all-facts ((?f hair)) ;; Loop through all matching grayhair facts
            (eq ?f:name grayhair)
            (retract ?f)) ;; Retract the old grayhair fact
         (assert (hair (name grayhair) (value ?new-value))) ;; Assert the updated grayhair fact
         (printout t "grayhair updated to: " ?new-value crlf)
      else
         (printout t "Error: grayhair fact not found!" crlf)
   )
)

;; Function to change the lightcolor fact
(deffunction change-lightcolor(?new-value)
   "Changes the value of the lightcolor"
   (bind ?lightcolor-facts (find-all-facts ((?f hair)) (eq ?f:name lightcolor))) ;; Get all matching lightcolor facts
   (if (neq (length$ ?lightcolor-facts) 0) ;; Check if any lightcolor fact exists
      then
         (do-for-all-facts ((?f hair)) ;; Loop through all matching lightcolor facts
            (eq ?f:name lightcolor)
            (retract ?f)) ;; Retract the old lightcolor fact
         (assert (hair (name lightcolor) (value ?new-value))) ;; Assert the updated lightcolor fact
         (printout t "lightcolor updated to: " ?new-value crlf)
      else
         (printout t "Error: lightcolor fact not found!" crlf)
   )
)

(deffunction main ()
   (reset)
   (run)
   (printout t "You can take haircut" crlf))