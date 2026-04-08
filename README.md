# Sistema de Gestión de Matrículas SML

![Lenguaje](https://img.shields.io/badge/Lenguaje-SML-blue)
![Estado](https://img.shields.io/badge/Estado-Finalizado-brightgreen)
![Versión](https://img.shields.io/badge/Versión-1.0-orange)
![Licencia](https://img.shields.io/badge/Licencia-Académico-lightgrey)

Sistema desarrollado en **Standard ML (SML)** que permite registrar y analizar matrículas universitarias a partir de archivos CSV.

Incluye funcionalidades de creación de registros y análisis de datos utilizando el paradigma funcional.

---

## 👤 Integrante

- **Alice Arias Salazar**

---

## 📚 Información académica

- **Curso:** Lenguajes de Programación  
- **Semestre:** I Semestre, 2026  
- **Proyecto:** Tarea Programada #2  
- **Fecha de entrega:** 15/04/2026  
- **Estatus:** Excelente  

---

## 📖 Descripción

Este sistema permite gestionar matrículas universitarias mediante el uso de archivos CSV, facilitando tanto el registro como el análisis de la información.

El programa se divide en dos componentes principales: un creador de registros y un analizador de datos, aplicando principios del paradigma funcional como el uso de listas, tuplas y funciones de orden superior.

---

## ⚙️ Funcionalidades

###  Creador
- Agregar nuevas matrículas al archivo CSV  
- Registro de datos: carnet, nombre, curso, créditos y costo  
- Limpieza completa del catálogo  

###  Analizador
- Ranking de cursos con mayor ingreso  
- Cursos con más de 5 estudiantes  
- Búsqueda por estudiante (nombre o carnet)  
- Filtrado de cursos por créditos  
- Reporte general del sistema  

---

##  Características técnicas

- Uso de listas y tuplas  
- Programación funcional  
- Funciones de orden superior (`map`, `filter`, `foldl`)  
- Manejo de archivos CSV  
- Procesamiento de datos  

---

## ▶️ Ejecución

```bash
sml
use "main.sml";
main();
