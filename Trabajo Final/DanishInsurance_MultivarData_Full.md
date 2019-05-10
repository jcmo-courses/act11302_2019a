# _Bulding Insurance Data_

## Descripción

Los datos corresponden a los montos de siniestros de un portafolio de seguros contra daños por incendio en edificios comerciales, correspondiente al periodo entre `1980` y `1990`. La cobertura de este seguro contempla tres tipos de daños: 

a. Daño al edificio, 

b. Daño al contenido, y

c. Daño por pérdida de ventas. 

Los `2,167` registros corresponden a los daños de distintos edificios. Todos estos los daños son sujetos a estar cubiertos por el `contrato de seguro`.

Las unidades monetarias de los daños estan regitradas en escala logaritmica, y han sido homologados a la misma base monetaria.

## Diccionario

El contenido de los datos se describe a continuación:

- `date` DATE - Fecha.

- `building` FLT - Monto del daño al edificio.

- `contents` FLT - Monto del daño al contenido del edificio.

- `profits` FLT - Monto del daño por pérdida de ventas. 

- `total` FLT - Monto agregado del daño.

**Nota:** Algunos de estos siniestros tuvieron imparto sólo en algunas de las tres coberturas del seguro. Las coberturas no incluidas estan regsitradas como (`building`==0), (`contents`==0) o (`profits`==0).
