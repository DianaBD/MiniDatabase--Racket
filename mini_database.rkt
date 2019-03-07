(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(struct column ( name values )) ; values = lista de valori
(struct tabela ( name columns )) ; columns = lista de coloane
(struct database ( tables )) ; tables = lista de tabele

(define init-database
  (λ ()
    (database '())
    ))

(define create-table
  (λ (table columns-name)
    (tabela table (foldr (λ( col acc )
                      (cons (column col '()) acc))
                    '() columns-name))
    ))

(define get-name
  (λ (table)
    (tabela-name table)
    ))

(define get-columns
  (λ (table)
    (foldr (λ( col acc )
             (cons (column-name col) acc))
           '() (tabela-columns table)))
    )

(define get-column
  (λ (table columnname)
    (if (null? (filter (λ (col)
              (equal? columnname (column-name col)))
            (tabela-columns table)))
        null
        (first (filter (λ (col)
              (equal? columnname (column-name col)))
            (tabela-columns table))))
    ))

(define get-tables
  (λ (db)
    (database-tables db)
    ))

(define get-table
  (λ (db table-name)
    (if (null? (filter (λ (tab)
              (equal? table-name (get-name tab)))
            (get-tables db)))
        null
        (first (filter (λ (tab)
              (equal? table-name (get-name tab)))
            (get-tables db))))
    ))

(define add-table
  (λ (db table)
     (database (cons table (database-tables db)))
   ))

(define remove-table
  (λ (db table-name)
    (database (remove (get-table db table-name) (get-tables db))) 
    ))


;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db (database (list (tabela "Studenți" (list (column "Număr matricol" '(123 124 125 126))
                                                    (column "Nume" '("Ionescu" "Popescu" "Popa" "Georgescu"))
                                                    (column "Prenume" '("Gigel" "Maria" "Ionel" "Ioana"))
                                                    (column "Grupă" '("321CA" "321CB" "321CC" "321CD"))
                                                    (column "Medie" '(9.82 9.91 9.99 9.87))
                                                    ))
                           (tabela "Cursuri" (list (column "Anul" '("I" "II" "III" "IV" "I" "III"))
                                                   (column "Semestru" '("I" "II" "I" "I" "II" "II"))
                                                   (column "Disciplină" '("Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date"))
                                                   (column "Număr credite" '(5 6 5 6 5 5))
                                                   (column "Număr teme" '(2 3 3 3 3 0))
                                                   ))
                           )))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

; cols = lista de coloane
; rec = lista de perechi in genul (numecol . value)
; caar rec = numele coloanei din prima pereche
; cdar rec = valoarea pt coloana din prima pereche
; column-values (car cols) = lista valori prima coloana din cols
; rez = lista de coloane updatate

(define insert
  (λ (db table-name record)
    (database
     (cons
      (tabela table-name (reverse 
                  (let put ( (cols (tabela-columns (get-table db table-name)))
                             (rec record)
                             (rez '()))
                    (if (null? cols)
                        ; am trecut prin toate coloanele tabelului => returnez lista cu coloanele updatate
                        rez
                        ; mai sunt coloane de completat => verific daca exista pereche pt coloana curenta
                        (if (not (empty? (filter (λ (coln)
                                                  (equal? (column-name (car cols)) (car coln)))
                                                rec)))
                            
                            ; exista pereche => adaug coloana updatata la rezultat si trec la urmatoarea pereche
                            (put (cdr cols)
                                 rec
                                 (cons (column (column-name (car cols))
                                               (reverse (cons
                                                         (cdr (car (filter (λ (coln)
                                                                              (equal? (column-name (car cols)) (car coln)))
                                                                            rec)))
                                                         (reverse (column-values (car cols))))))
                                       rez) )
                            ; nu exista pereche => completez cu NULL si nu trec la urm pereche 
                            (put (cdr cols)
                                 rec
                                 (cons (column (column-name (car cols))
                                               (reverse (cons NULL (reverse (column-values (car cols))))))
                                       rez) )
                            )
                        ) ;end if (null? cons)
                    ) ;end let put
                  ) ; reverse
                ) ;end create-table
      ; concatenez tabelul modificat la restul tabelelor
      (filter-not (λ (tab)
              (equal? table-name (get-name tab)))
            (get-tables db))
      ) ;end cons
     ) ;end database
    ))


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

(define simple-select
  (λ (db table-name columns)
    (if (empty? (tabela-columns (get-table db table-name)))
        '()
        (foldr (λ( wantedcol acc)
             ; adaug la rezultat lista de valori a coloanei din tabel care are numele wantedcol
              (cons (column-values (car (filter (λ (col)
                         (equal? (column-name col) wantedcol))
                       (tabela-columns (get-table db table-name)))))
                    acc))
           '() columns))
           
    ))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================


(define make-intersect
  (λ ( results )
    (foldr (λ (L intersection)
             (filter (λ (el)
                       (member el L))
                     intersection))
           (car results) (cdr results))
    ))
           
             

;colname = numele coloanei din care vrem sa selectam doar valorile care indeplinesc simultan conditiile date
;conditions = lista de conditii
;rezults = va fi o lista de liste, prima lista avand valorile din colname care indeplinesc conditia1,
;   a doua lista va avea valorile care indeplinesc conditia2, etc
;   => se face intersectie intre listele din rezults => obtin toate valorile din colname care indeplinesc simultan conditiile date

;cond = (comparatorul  nume_coloana_conditie valoarea_de_comparatie)
;(first cond) = comparatorul din conditie
;(second cond) = numele coloanei din conditie
;(third cond) = valoarea de comparatie din conditie
;(first row) = valoarea pt coloana din care selectam
;(second row) = valoarea pt coloana din conditie

; operatie select pentru o singura coloana si lista de conditii
(define select-values
  (λ (db tablename colname conditions)
    ; apply primeste lista rezults, adica '( (lista valori din colname pt cond1),(lista valori din colname pt cond2)...)
    (if (empty? conditions)
       (filter (λ (x)
                 (not (equal? x NULL)))(column-values
                                  (car (filter (λ (col)
                                                 (equal? colname (column-name col)))
                                               (tabela-columns (get-table db tablename))))))
        (make-intersect
           ; iau fiecare conditie pe rand si adaug la rezults lista de valori din coloana colname care o indeplinesc
           (foldr (λ (cond rezults)
                    ; iau fiecare rand din coloana colname si coloana din conditie si verific daca valoarea din
                    ; coloana de selectie indeplineste cond
                    (cons (reverse (foldl (λ (row acc)
                                            (if (or (equal? (first row) 'null) (equal? (second row) 'null))
                                                acc
                                                (if ((first cond) (second row) (third cond))
                                                    (cons (first row) acc)
                                                    acc)))
                                 ;aplic foldr pe liniile din tabel doar pt coloana de selectie si cea din conditie
                                 '() (apply map list (simple-select db tablename  (list colname (second cond))))))
                          ;cons rezultatul pentru conditia curenta la rezults
                          rezults))
                  '() conditions) ; primul foldr
           )
        );if
    ))
                                 
(define trunchiaza
  (λ (columns-values-list)
    (map (λ (x)
           (take x (apply min ( map (λ (L)
                                    (length L)) columns-values-list)) )) columns-values-list)
    ))

(define select
  (λ (db table-name columns conditions)
    
     (foldr (λ (from rez)
             (if (pair? from)
                 (cond
                   ((equal? 'min (car from)) (cons  (apply min (select-values db table-name (cdr from) conditions)) rez))
                   ((equal? 'max (car from)) (cons (apply max (select-values db table-name (cdr from) conditions)) rez))
                   ((equal? 'count (car from)) (cons (length (remove-duplicates (select-values db table-name (cdr from) conditions))) rez))
                   ((equal? 'sum (car from)) (cons (foldl + 0 (select-values db table-name (cdr from) conditions)) rez))
                   ((equal? 'avg (car from)) (cons (exact-round (/ (exact->inexact (foldl + 0 (select-values db table-name (cdr from) conditions)))
                                                      (length (select-values db table-name (cdr from) conditions))))
                                                   rez))
                   ((equal? 'sort-asc (car from)) (cons (sort (select-values db table-name (cdr from) conditions)
                                                              (cond
                                                                ((number? (car (select-values db table-name (cdr from) conditions))) <)
                                                                ((string? (car (select-values db table-name (cdr from) conditions))) string<?))) rez))
                   ((equal? 'sort-desc (car from)) (cons (sort (select-values db table-name (cdr from) conditions)
                                                              (cond
                                                                ((number? (car (select-values db table-name (cdr from) conditions))) >)
                                                                ((string? (car (select-values db table-name (cdr from) conditions))) string>?))) rez)))
                 ; else - daca nu este pereche (operatie. coloana)
                 (cons (select-values db table-name from conditions) rez)))
           '() columns) ;end foldr                                     
    ))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================

(define select-indexes
  (λ (db tablename colname conditions)
    ; apply primeste lista rezults, adica '( (lista valori din colname pt cond1),(lista valori din colname pt cond2)...)
    (if (empty? conditions)
      (build-list (length (column-values (get-column (get-table db tablename) colname))))
      (make-intersect
           ; iau fiecare conditie pe rand si adaug la rezults idicii valorilor din coloana colname care o indeplinesc
           (foldr (λ (cond rezults)
                    (cons (indexes-where (column-values (get-column (get-table db tablename) (second cond))) (λ(val)
                                                                                                         ((first cond) val (third cond))))
                                         
                          ;cons rezultatul pentru conditia curenta la rezults
                          rezults))
                  '() conditions) ; primul foldr
           ))))
       


(define update
  (λ (db table-name values conditions)
    (database
     (cons
      (tabela table-name
              ; foldr pe coloanele vechi -> modific unde e cazul si intorc lista de coloane
              (foldl (λ(col result)
                       (if (assoc (column-name col) values)
                           (cons (column (column-name col)
                                         ; foldr pe valorile vechi -> acc va fi lista cu valorile coloanei curente 
                                         (foldr (λ (p acc)
                                                  (if (member (car p) (select-indexes db table-name (column-name col) conditions))
                                                      (cons (cdr (assoc (column-name col) values)) acc)
                                                      (cons (second p) acc)))
                                                '() (map list (range (length (column-values col))) (column-values col) )))
                                 result)
                           (cons col result)))
                       '() (tabela-columns (get-table db table-name))))
      (filter-not (λ (tab)
                    (equal? table-name (get-name tab)))
                  (get-tables db))
      ) ;end cons
     ) ;end database
    ))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================

(define get-filtered-column
  (λ (db table-name conditions col)
    (column (column-name col)
            (foldr (λ (p acc)
                     (if (member (car p) (select-indexes db table-name (column-name col) conditions))
                         acc
                         (cons (second p) acc)))
                   '() (map list (range (length (column-values col))) (column-values col) )))
    ))

(define delete
  (λ (db table-name conditions)
   (database
     (cons
      (tabela table-name
              ; foldr pe coloanele vechi -> sterg unde e cazul si intorc lista de coloane
              (filter (λ (el)
                        (not (empty? (column-values el ))))
                      (foldl (λ(col result)
                               (cons (get-filtered-column db table-name conditions col) result))
                             '() (tabela-columns (get-table db table-name)))))
      (filter-not (λ (tab)
                    (equal? table-name (get-name tab)))
                  (get-tables db))
      ) ;end cons
     ) ;end database
    ))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================

(define needed-columns
 (λ (db tablename columnnames)
   (foldr (λ (col acc)
            (cons col acc))
          '()(filter (λ (coln)
                       (member coln (get-columns (get-table db tablename)))) columnnames ))
   ))

(define common-column
  (λ (db tabnames)
    (car (make-intersect (map (λ (tabn)
                              (get-columns (get-table db tabn))) tabnames)))
    ))

;args = '(com db tabname coumns-to-select conditions)
(define grupeaza
  (λ (args)
     (filter (λ (L)
               (member (car (car L)) (car args)))
             (group-by car (sort (apply map list (trunchiaza (apply select (cdr args))))  (lambda (x y)
                                                                                            (< (car x) (car y)))  )))
    ))

(define cartezian
  (λ (db tabn1 tabn2 colns1 colns2 conditions com1 com2)
    (apply map list (foldl append
                           ; acumulator -> prima lista
                           (last (map cartesian-product (grupeaza (list com1 db tabn1 colns1 conditions)) (grupeaza (list com2 db tabn2 colns2 conditions))))
                           ; restul listelor
                           (cdr (reverse (map cartesian-product (grupeaza (list com1 db tabn1 colns1 conditions)) (grupeaza (list com2 db tabn2 colns2 conditions))))))
           )))

;args = '( db tabn1 tabn2 colns1 colns2 conditions com)
(define almost
  (λ (args)
    (foldr (λ (L acc)
           (cons (apply map list L) acc)) '() (foldr (λ (T acc)
                                                       (cons (map (λ (L)
                                                                    (cdr L)) T) acc))
                                                     '()(apply cartezian args)))
    ))

; scap de coloana comuna 
(define finally
  (λ (args)
    (if (car args)
        (append (first (almost (cdr args))) (cdr (last (almost (cdr args)))))
        (append (first (almost (cdr args))) (last (almost (cdr args))))
    )))
                                      
(define natural-join
  (λ (db tables columns conditions)
    (finally (list (member (common-column db tables) columns)
                   db
                   (first tables)
                   (second tables)
                   (cons (column-name (get-column (get-table db (first tables)) (common-column db tables))) (needed-columns db (first tables) columns))
                   (cons (column-name (get-column (get-table db (second tables)) (common-column db tables))) (needed-columns db (second tables) columns))
                   conditions
                   (filter (λ (x)
                             (and (not (equal? NULL x)) (member x (column-values (get-column (get-table db (second tables))(common-column db tables))) ))) (column-values (get-column (get-table db (first tables)) (common-column db tables))))
                   (filter (λ (x)
                             (and (not (equal? NULL x)) (member x (column-values (get-column (get-table db (first tables)) (common-column db tables))) ))) (column-values (get-column (get-table db (second tables)) (common-column db tables))))
    ))
  ))