; заготовка "Доктора". Сентябрь 2023
#lang scheme/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name)  ;заменено doctor-driver-loop на doctor-driver-loop-v2
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name) 
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 0 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ

      )
)

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (thank you for trusting me and sharing this with me)
                              (i am here so that you can freely talk about all your feelings)
                              (what did you feel at that moment?))
         )
)

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (if i understand you correctly, your problem is that)
                                       (at what point did you realize that)
                                       (you said that))
                )
                (change-person user-response)
        )
 )

; замена лица во фразе
(define (change-person phrase)
        (many-replace-v3    ;many-replace заменена на many-replace-v2,
	'((am are)          ;а many-replace-v2 земеннена на many-replace-v3
        (are am)
        (i you)
        (me you)
        (mine yours)
        (my your)
        (myself yourself)
        (you i)
        (your my)
        (yours mine)
        (yourself myself)
        (we you)
        (us you)
        (our your)
        (ours yours)
        (ourselves yourselves)
        (yourselves ourselves)
        (shall will))
                      phrase)
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)


; Упражнения 2. Напишите новую версию функции many-replace-v2 с хвостовой рекурсией
(define (many-replace-v2 replacement-pairs lst)
  (let loop ((lst lst) (result '()))
        (if (null? lst) (reverse result) ;просмотрели весь список
            (let ((pat-rep (assoc (car lst) replacement-pairs))) ;цикл по словам
                     (loop (cdr lst) 
                          (cons (if pat-rep (cadr pat-rep) (car lst))
                                result)
                     )
            )
        )
   )
)

;Упражнения 3. Напишите ещё одну версию функции many-replace-v3 через map
(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (x)
            (let ((pat-rep (assoc x replacement-pairs)))
              (if pat-rep (cadr pat-rep) x)
            )
        )
       lst
   )
)

;(change-person '(i feel angry))


;Упражнение 4. 3-я стратегия генерации ответов
(define empty-answers #())

(define (answers-empty? answers) (if (equal? answers empty-answers) #t #f))

(define (insert answers response)
  (let ((length (vector-length answers)))
  (define (ins x) (if (= x 0) response (vector-ref answers (sub1 x))))
  (if (< length 5)
      (build-vector (add1 length)  ins)
      (build-vector 5 ins))))
  

(define (doctor-driver-loop-v2 name)
  (let driver-loop ((answers empty-answers))
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply-v2 user-response answers)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (driver-loop (insert answers user-response))
             )
       )
      )
   )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply-v2 user-response answers)
  (let ((n (if (answers-empty? answers) 3 4)) ; если есть предыдущий ответ, то расширяем диапозон ответов до 4
        (m (if (contain-keyword? user-response) 0 1))) ; если есть ключевое слово в реплике, то расширяем от 0
    (case (random m n) ; с равной вероятностью выбирается один из трех способов построения ответа
          ((0) (keyword-answer user-response)) ; 4й способ
          ((1) (hedge-answer))  ; 1й способ
          ((2) (qualifier-answer user-response)) ; 2й способ
          ((3) (history-answer answers)) ; 3й способ
      )
   )
)

; 3й способ генерации ответной реплики -- замена лица в одной из предыдущих реплик пользователя и приписывание к результату "earlier you said that"
(define (history-answer answers)
  (let ((response (vector-ref answers (random 0 (vector-length answers)))))
    (append '(earlier you said that)
                (change-person response)
    )
  )
)

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))


; Блок 2
; Упражнение 5. Многопользовательский «Доктор»
(define (ask-patient-name)
 (begin
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)

(define (visit-doctor-v2 stopWord numPatients)
  (if (= numPatients 0) (print '(time to go home))
      (let ((name (ask-patient-name)))
        (if (equal? name stopWord) (print '(time to go home))
            (begin
              (printf "Hello, ~a!\n" name)
              (print '(what seems to be the trouble?))
              (doctor-driver-loop-v2 name)
              (newline)
              (visit-doctor-v2 stopWord (sub1 numPatients))
            )
         )
      )
   )
)


; Упражнение 6. 4-я стратегия генерации ответов
(define keywords-structure '#(
  #( ; начало данных 1й группы
    #(depressed suicide exams university) ; список ключевых слов 1й группы
    #( ; список шаблонов для составления ответных реплик 1й группы 
	  (when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)
	)
  ) ; завершение данных 1й группы
  #( ; начало данных 2й группы ...
    #(mother father parents brother sister uncle aunt grandma grandpa)
    #(
	  (tell me more about your * , i want to know all about your *)
      (why do you feel that way about your * ?)
	)
  )
  #(
    #(university scheme lections)
	#(
	  (your education is important)
	  (how much time do you spend on your studies ?)
	)
  )
  #(
    #(anger rage joy happiness)
	#(
	  (what is the cause of * ?)
	  (what else do you feel ?)
	)
  )
  #(
    #(rain snow sun)
	#(
	  (what does the weather matter to you ?)
	  (what do you feel about the * ?)
	)
  )
))

; предобработка keywords-structure --> список уникальных ключевых слов - keywords
(define (add-word i result word)
  (if (member word result) result (cons word result)))

(define (add-words-from-vector i result vec)
  (vector-foldl add-word result (vector-ref vec 0)))

(define keywords (vector-foldl add-words-from-vector '() keywords-structure))


; предикат для проверки реплики на наличие хотяб одного ключевого слова
(define (contain-keyword? user-response)
  (call/cc (lambda (cc-exit)
     (let loop ((lst user-response))
      ; (if (null? lst) (print 'stop) (print (car lst))) (newline)
       (cond ((null? lst) #f)
             ((member (car lst) keywords) (cc-exit #t))
             (else (loop (cdr lst)))
       )
     )
   ))
)

; 4 способ генерации ответа
(define (build-list-keywords user-response) ; строит список ключевых слов в реплике (с повторами) 
  (filter (lambda(x) (member x keywords)) ; можно сделать через фильтр
          user-response))

(define (pick-random-list lst) ; выдает случайный элемент из списка
  (list-ref lst (random 0 (length lst))))

(define (random-keyword-selection user-response) ; выдает случайное ключевое слово в реплике
  (pick-random-list (build-list-keywords user-response)))

(define (build-list-templates keyword) ; строит список из шаблонов для ключевого слова
  (vector-foldl (lambda (i result group)
                  (if (vector-member keyword (vector-ref group 0))
                      (vector-foldl (lambda (j res template) (cons template res)) result (vector-ref group 1))
                      result
                  ))
                 '() keywords-structure
  )
)
  
(define (replace-stars keyword lst) ; заменяет в шаблоне * на ключевое слово
  (map (lambda (x)
              (if (equal? x '*) keyword x)
       )
       lst
   )
) 
  
(define (keyword-answer user-response) ; 4й способ генерации ответа, по ключевому слову --> шаблон
  (let ((keyword (random-keyword-selection user-response)))
    (replace-stars keyword (pick-random-list (build-list-templates keyword)))))

