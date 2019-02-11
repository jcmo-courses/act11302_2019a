# _AllState Car Insurance Data_

## Descripcion

Los datos corresponden a reclamos por polizas de un seguro de autos de la empresa _AllState_. Se supone que cada poliza es contratada por un hogar. Los montos de reclamos para las diferentes polizas pueden diferenciarse por anio de suscripcion, modelo del vehiculo y fabricante.

## Diccionario

- `Household_ID` INT - Identificador numerico de la poliza (una por hogar).

- `Vehicle` INT - identificador del vehiculo asegurado por poliza (dentro de cada hogar).

- `Calendar_Year` INT - Anio calendario en el que el vehiculo fue asegurado.

- `Model_Year` INT - Anio calendario de fabricacion/venta del vehiculo. 

- `Blind_Make` INT - Fabricante del vehiculo (discresional).

- `Claim_Amount` INT - Monto de reclamo asociado con el vehiculo (montos en USD).

**Nota:** Las polizas con reclamos nulos (`Claim_Amount`==0) corresponden a casos no siniestrados.