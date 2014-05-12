(ns c6502.macros)

(defmacro defopcodes
  [impl table]
  `(do
     ~@(map (fn [[opc addressing]]
              `(defmethod ~'opcode ~opc [cpu#] (~impl cpu# (~addressing cpu#)))) table)))

