;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-defs)

(def (class* eas) df-flagarray (array-item data-field concrete-item)
  ()
  (:default-initargs :type-name $flag-bit))

(def (class* eas) df-array (array-item data-field concrete-item)
  ())

(def (class* eas) df-linked-list (sequence-item data-field concrete-item)
  ())

(in-package :cl-linux-debug.data-info)

;; df-flagarray implementation

(defmethod compute-effective-fields (context (type df-flagarray))
  (list
   (make-instance 'pointer :name $start)
   (make-instance 'int32_t :name $size)))

(defmethod array-base-dimensions ((type df-flagarray) ref)
  (assert (typep (effective-contained-item-of type) 'flag-bit))
  (let ((s $ref.start) (e $ref.size))
    (awhen (and s e)
      (values (start-address-of s) (* 8 e)))))

;; df-array implementation

(defmethod compute-effective-fields (context (type df-array))
  (list
   (make-instance 'pointer :name $start)
   (make-instance 'int16_t :name $size)))

(defmethod array-base-dimensions ((type df-array) ref)
  (let ((s $ref.start) (e $ref.size))
    (awhen (and s e)
      (values (start-address-of s) e))))

(defmethod build-set-array-base-dimensions (context (node df-array) offset ctx ptr-var cnt-var)
  (with-walker-utils (u ctx offset)
    `(let* ((start ,(u/field-int node $start 4))
            (size ,(u/field-int node $size 2)))
       (when (< size most-positive-fixnum)
         (setf ,ptr-var start ,cnt-var size)))))

;; df-linked-list implementation

(defmethod compute-effective-fields (context (type df-linked-list))
  (assert (type-name-of type))
  (list
   (make-instance 'compound :name $head :type-name (type-name-of type))))

(defmethod sequence-content-items ((type df-linked-list) ref)
  (loop for cur = $ref.head.next then $cur.next
     while (valid-ref? cur)
     collect @cur.item into items
     finally (return (coerce items 'vector))))

(in-package :cl-linux-debug.data-xml)

(defun find-entity (key) (find-by-id $global.world.entities.all $id key))
(defun find-unit (key) (find-by-id $global.world.units.all $id key))
(defun find-item (key) (find-by-id $global.world.items.all $id key))
(defun find-nemesis (key) (find-by-id $global.world.nemesis.all $id key))
(defun find-artifact (key) (find-by-id $global.world.artifacts.all $id key))
(defun find-building (key) (find-by-id $global.world.buildings.all $id key))
(defun find-activity (key) (find-by-id $global.world.activities.all $id key))
(defun find-squad (key) (find-by-id $global.world.squads.all $id key))

(defun find-plant-raw (key) $global.world.raws.matgloss_plant[key])
(defun find-creature (key) $global.world.raws.creatures[key])
(defun find-figure (key) (find-by-id $global.world.history.figures $id key))

(defun item-subtype-target (type subtype)
  (let* ((defs $global.world.raws.itemdefs)
         (key (enum-to-key $item_type type))
         (table (case key
                  ($WEAPON $defs.weapons)
                  ($TRAPCOMP $defs.trapcomps)
                  ($TOY $defs.toys)
                  ($TOOL $defs.tools)
                  ($INSTRUMENT $defs.instruments)
                  ($ARMOR $defs.armor)
                  ($AMMO $defs.ammo)
                  ($SIEGEAMMO $defs.siege_ammo)
                  ($GLOVES $defs.gloves)
                  ($SHOES $defs.shoes)
                  ($SHIELD $defs.shields)
                  ($HELM $defs.helms)
                  ($PANTS $defs.pants)
                  ($FOOD $defs.food))))
    $table[subtype]))

(defun matgloss-target (type subtype)
  (let* ((raws $global.world.raws)
         (key (enum-to-key $item_type type))
         (table (case key
                  ($WOOD $raws.matgloss_wood)
                  ($STONE_GRAY $raws.matgloss_stone)
                  ($STONE_LIGHT $raws.matgloss_stone)
                  ($STONE_DARK $raws.matgloss_stone)
                  ($GEM_ORNAMENTAL $raws.matgloss_gem)
                  ($GEM_SEMI $raws.matgloss_gem)
                  ($GEM_PRECIOUS $raws.matgloss_gem)
                  ($GEM_RARE $raws.matgloss_gem)
                  ($BONE $raws.creatures)
                  ($IVORY $raws.creatures)
                  ($HORN $raws.creatures)
                  ($PEARL $raws.creatures)
                  ($SHELL $raws.creatures)
                  ($LEATHER $raws.creatures)
                  ($SILK $raws.creatures)
                  ($PLANT $raws.matgloss_plant)
                  ($RENDERED_FAT $raws.creatures)
                  ($SOAP_ANIMAL $raws.creatures)
                  ($FAT $raws.creatures))))
    $table[subtype]))

(defun name-has-substring? (name substring)
  (some @$(search substring $) (describe-obj name)))
