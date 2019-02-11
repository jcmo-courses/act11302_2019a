# Datos de fraude crediticio

## Datos

`d4a_frad_data.csv` - Muestra con 100 mil registros de credito de un banco particular.

-- Los datos corresponden a registros reales confidenciales de un banco dado en EE.UU. --

## Diccionario

- `custID` INT - Llave primaria. ID clliente. 

- `gender` INT - Genero del cliente (`1` - Hombre; `2` Mujer)

- `state` INT - Estado de EE.UU. donde reside el cliente.

- `cardholder` INT - Identificador del tarjetabiente (`1` Titular; `2` Adicional)

- `balance` INT - Balance de la cuenta en USD al termino de un periodo dado.

- `numTrans` INT - Numero de transacciones domesticas en un periodo dado.

- `numIntlTrans` INT - Numero de transacciones internacionales en un periodo dado.

- `creditLine` INT - Indicador del tipo de linea de credito de la cuenta (`id` es un entero de 1 a 20 tipos de cuentas)

- `fraudRisk"` INT - Calificador de quebranto (`1` Riesgo quebranto; `0` No riesgo quebranto)
