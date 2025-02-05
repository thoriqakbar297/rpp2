//using initial-fact to call first rule to get the user gender 
(defrule read-gender
    (initial-fact)
    =>
    (printout t crlf crlf "Welcome! Diet and Nutrition Expert System" crlf)
    (printout t " ****************************************** " crlf)
    (printout t " This output of this program is" crlf)
    (printout t " 1. Your Body Mass Index (BMI) and body-status." crlf)
    (printout t " 2. Recommended daily calories needed based on your body-status." crlf)
    (printout t " 3. Daily protein needed based on your weight (kgs)." crlf)
    (printout t " 4. Daily celcium needed based on your age." crlf)
    (printout t " 5. Daily fiber needed based on your calories needed." crlf)
    (printout t " 6. Daily carbohydrate needed based on your weight (kgs)." crlf)
    (printout t "  **********************" crlf crlf)
    (printout t "What is your gender (Female/Male) *case-sensitive* :") 
    (assert (gender (read))))

//after gender is initialize, get user age
(defrule read-age
    (gender ?)
    =>
    (printout t "Please enter your age:") 
    (assert (age (read))))

// followed by user' height 
(defrule read-height
    (age ?)
    =>
    (printout t "Please enter your weight (in KGs):")
    (assert (weight (read))))

// followed by the number of day that user exercise for a week 
(defrule read-activity-days
    (weight ?)
    =>
    (printout t "How many day do you exercise for a week:")
    (assert (activity-days (read))))

// if the number of day that user exercise for a week is less than 2 days, 
// set the activity rate to sedentary
// and set activity factor to 1.2 for calculation later
(defrule activity-sedentary
    (activity-days ?d)
    =>
    (if (< ?d 2)
    then
    (assert (activity-factor 1.2) (activity-rate sedentary))))

// if the number of day that user exercise for a week is equal to or more than 2 and less than 5 days,
// set the activity rate to moderate
// and set activity factor to 1.55 for calculation later
(defrule activity-moderate
    (activity-days ?d)
    =>
    (if (>= ?d 2) 
    then
    (if (< ?d 5)
    then
    (assert (activity-factor 1.55) (activity-rate moderate)))))

//if the number of day that user exercise for a week is equal more than 5 days, 
//set the activity rate to hard
//and set activity factor to 1.75 for calculation later
(defrule activity-hard
    (activity-days ?d)
    =>
    (if (>= ?d 5)
    then
    (assert (activity-factor 1.75) (activity-rate hard)))) 

//calculating Body Mass Index (BMI)
(defrule calculate-BMI
    (weight ?w) 
    (height ?h)
    =>
    (assert (bmi (precision (/ ?w (*(/ ?h 100) (/ ?h 100))) 2))))

//set body-status to underweight if bmi is less than 18.5
(defrule status-underweight
    (bmi ?b)
    =>
    (if (< ?b 18.5)
    then
    (assert (body-status underweight))))

//set body-status to normal-weight if bmi is equal to or more than 18.5 and less than 24.9 
(defrule status-normalweight
    (bmi ?b)
    =>
    (if (>= ?b 18.5) 
    then
    (if (< ?b 24.9)
    then
    (assert (body-status normal-weight)))))

//set body-status to overweight if bmi is equal to or more than 24.9 and less than 29.9
(defrule status-Overweight
    (bmi ?b)
    =>
    (if (>= ?b 24.9)
    then
    (if (< ?b 29.9)
    then
    (assert (body-status overweight)))))

//set body-status to obesity if bmi is equal to or more than 29.9
(defrule status-obesity
    (bmi ?b)
    =>
    (if (>= ?b 29.9)
    then
    (assert (body-status obesity))))

//calculating daily calories needed for female 
(defrule calculate-calories-needed-female
    (gender Female)
    (age ?a) 
    (weight ?w) 
    (height ?h)
    (activity-factor ?af)
    =>
    (assert (calories-needed (* (- (- (+ (* 10 ?w) (* 6.25 ?h)) (* 5 ?a)) 161) ?af))))

//calculating daily calories needed for male
(defrule calculate-calories-needed-Male
    (gender Male)
    (age ?a) 
    (weight ?w)
    (height ?h)
    (activity-factor ?af)
    =>
    (assert (calories-needed (*(+ (- (+ (* 10 ?w) (* 6.25 ?h)) (* 5 ?a)) 5) ?af))))

//set daily calcium needed for age less than 4
(defrule get-calcium-baby
    (age ?a)
    =>
    (if (< ?a 4)
    then
    (assert (calcium-needed 700mg))))

//set daily calcium needed for age more than or equal to 4 but less than 9 
(defrule get-calcium-children
    (age ?a)
    =>
    (if (>= ?a 4)
    then
    (if (< ?a 9)
    then
    (assert (calcium-needed 1000mg)))))

//set daily calcium needed for age more than or equal to 9 but less than 19 
(defrule get-calcium-teenager
    (age ?a)
    =>
    (if (>= ?a 9)
    then
    (if (< ?a 19)
    then
    (assert (calcium-needed 1300mg)))))

//set daily calcium needed for age more than or equal to 19 but less than 51 
(defrule get-calcium-adult
    (age ?a)
    =>
    (if (>= ?a 19)
    then
    (if (< ?a 51)
    then
    (assert (calcium-needed 1000mg)))))

//set daily calcium needed for age more than or equal to 51
(defrule get-calcium-oldpeople
    (age ?a)
    =>
    (if (>= ?a 51)
    then
    (assert (calcium-needed 1200mg))))

//calculate daily protein needed based on weight and activity-rate 
(defrule get-protein-sedentary
    (activity-rate sedentary)
    (weight ?w)
    =>
    (assert (protein-needed (* 0.5 ?w))))

//calculate daily protein needed based on weight and activity-rate 
(defrule get-protein-moderate
    (activity-rate moderate) 
    (weight ?w)
    =>
    (assert (protein-needed (* 0.7 ?w))))

//calculate daily protein needed based on weight and activity-rate 
(defrule get-protein-hard
    (activity-rate hard)
    (weight ?w) 
    =>
    (assert (protein-needed (* 0.9 ?w))))

//calculate daily carbohydrate needed based on weight
(defrule get-carbohydrate
    (weight ?w)
    =>
    (assert (carbohydrate-needed (* 4 ?w))))

//calculate daily fiber needed based on how much calories the user daily needed 
(defrule get-fiber
    (calories-needed ?c)
    =>
    (assert (fiber-needed (float (precision (/ ?c 58.5) 2)))))

//printout
(defrule protein-advice
    (protein-needed ?p)
    =>
    (printout t crlf crlf "  ************ Result ************" crlf)
    (printout t" 1. You need " ?p "g of protein per day." crlf))

(defrule carbohydrate-advice 
    (carbohydrate-needed ?c)
    =>
    (printout t" 3. You need " ?c "g of carbohydrate per day." crlf))

(defrule fiber-advice 
    (fiber-needed ?f)
    =>
    (printout t" 2. You need " ?f "g of fiber per day." crlf))

(defrule calcium-advice 
    (calcium-needed ?c)
    =>
    (printout t"4. You need " ?c" of calcium per day." crlf))

//printout advice for underweight user
(defrule calories-advice-underweight
    (body-status underweight) 
    (calories-needed ?c)
    (bmi ?bm)
    (body-status ?b)
    (carbohydrate-needed ?ca)
    (calcium-needed ?ce)
    =>
    (printout t" 5. Your Body Mass Index (BMI) is " ?bm "(" ?b ")," crlf " 6. You need " ?c " calories per day. " crlf " 7. For advice from the experts, You may need extra daily 300 calories ("(+ 300 ?c)") to gain 0.25kg/week." crlf crlf))

//printout advice for normal-weight user 
(defrule calories-advice-normalweight
    (body-status normal-weight)
    (calories-needed ?c) 
    (bmi ?bm)
    (body-status ?b)
    (carbohydrate-needed ?ca)
    (calcium-needed ?ce)
    =>
    (printout t " 5. Your Body Mass Index (BMI) is " ?bm "(" ?b ")," crlf " need " ?c " calories per day to maintain your healthy weight." crlf crlf))

//printout advice for overweight user
(defrule calories-advice-overweight
    (body-status overweight)
    (calories-needed ?c) 
    (bmi ?bm)
    (body-status ?b)
    (carbohydrate-needed ?ca)
    (calcium-needed ?ce)
    =>
    (printout t" 5. Your Body Mass Index (BMI) is " ?bm "(" ?b")," crlf " need " ?c " calories per day. " crlf " 6. You need " ?c " calories per day. " crlf " 7. For advice from the experts, You may need to reduce your daily calories needed by 300 to (" (- ?c 300)") to loss 0.25kg/week." crlf crlf))


//printout advice for obesity user 
(defrule calories-advice-obesity
    (body-status obesity)
    (calories-needed ?c) 
    (bmi ?bm)
    (body-status ?b)
    (carbohydrate-needed ?ca) 
    (calcium-needed ?ce)
    =>
    (printout t " 5. Your Body Mass Index (BMI) is " ?bm "(" ?b "),"crlf " 6. You need " ?c " calories per day. " crlf "7. For advice from the experts, You may need to reduce your daily calories needed by 500 to (" (- ?c 300)") to loss 0.5kg/week." crlf crlf))




