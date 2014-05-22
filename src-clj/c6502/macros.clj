(ns c6502.macros)

(defmacro defopcodes
  [impl table]
  `(do
     ~@(map (fn [[opc addressing]]
              `(defmethod ~'opcode ~opc [cpu#]
                 (~impl cpu# (~addressing cpu#)))) table)))


(defmacro definstruction
  [impl table]
  `(do
     ~@(map (fn [[opc addressing]]
              `(defmethod ~'opcode ~opc [cpu#]
                 (let [p# (~addressing cpu#)]
                   (~impl (merge-with + cpu# (select-keys p# [:pc :cc])) (:addr p#))))) table)))
