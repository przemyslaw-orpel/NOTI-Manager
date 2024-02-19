# NOTI-Report
SAP ABAP Report for notification PM. 
Double click the location tree line displays open/processing/close notifications on the right side of the screen.

## Used technology: 
+  ABAP, OOP, GUI, EVENTS. 



## Custom Tabels

### ZPM_NOTI_TAB - Notification PM table type
| Line Type: | ZPM_NOTI |
| :-------- | :------- | 

### ZPM_NOTI - Notification PM structure
| Component | Typing Method| Component Type | Data Type | Length | Decimals | Short Description |
|------------|-------|------------|-----------|--------|----------|--------------------|
| MANDT      | Types | MANDT          | CLNT      | 3      | 0        | Client                                      |
| QMNUM      | Types | QMNUM          | CHAR      | 12     | 0        | Notification No                             |
| AUFNR      | Types | AUFNR          | CHAR      | 12     | 0        | Order Number                                |
| QMART      | Types | QMART          | CHAR      | 2      | 0        | Notification Type                           |
| QMTXT      | Types | QMTXT          | CHAR      | 40     | 0        | Short Text                                  |
| ARTPR      | Types | ARTPR          | CHAR      | 2      | 0        | Priority Type                               |
| PRIOK      | Types | PRIOK          | CHAR      | 1      | 0        | Priority                                    |
| ERNAM      | Types | ERNAM          | CHAR      | 12     | 0        | Name of Person who Created the Object       |
| ERDAT      | Types | ERDAT          | DATS      | 8      | 0        | Date on Which Record Was Created            |
| MZEIT      | Types | MZEIT          | TIMS      | 6      | 0        | Time of Notification                        |
| TPLNR      | Types | TPLNR          | CHAR      | 30     | 0        | Functional Location                         |
| EQUNR      | Types | EQUNR          | CHAR      | 18     | 0        | Equipment Number                            |


### ZPM_TREE - Tree table type
| Line Type: | ZPM_TREE |
| :-------- | :------- | 

### ZPM_TREE - Tree structure

| Component | Typing Method| Component Type | Data Type | Length | Decimals | Short Description |
|------------|-------|------------|-----------|--------|----------|--------------------|
| MANDT       | Types | MANDT          | CLNT      | 3      | 0        | Client                        |
| NODE_KEY    | Types | INT4       | INT4      | 10     | 0        | Natural Number                |
| NODE_PARENT | Types | INT4    | INT4      | 10     | 0        | Natural Number                |
| TPLNR       | Types | TPLNR          | CHAR      | 30     | 0        | Functional Location           |
| TPLMA       | Types | TPLMA          | CHAR      | 30     | 0        | Superior functional location  |
| PLTXT       | Types | PLTXT          | CHAR      | 40     | 0        | Description of functional location |
| EQUNR       | Types | EQUNR          | CHAR      | 18     | 0        | Equipment Number              |
| HEQUI       | Types | HEQUI          | CHAR      | 18     | 0        | Superordinate Equipment       |
| EQKTX       | Types | KTX01     | CHAR    | 40  |  0     | Description of technical object |


## PM Tables
| Table | Description                |
| :--------  | :------------------------- |
| `EQUI` | Equipment master data |
| `EQKT` | Equipment Short Texts |
| `EQUZ` | Equipment time segment |
| `ILOA` | PM Object Location |
| `IFLOT` | Functional Location / Short Texts |
| `QMIH` | Quality message - maintenance data excerpt |
| `QMEL` | Quality Notification |
| `JEST` | Individual Object Status |

## Text Symbols
| Code | Text                      | Length | Max Length|
|------|------------------------------------|---------|---------|
| 001  | Notification open             | 21      | 42      |
| 002  | Notification release         | 26      | 52      |
| 003  | Notification closed            | 23      | 46      |
| 004  | Fun. location / Equipment     | 30      | 40      |

## Design pattern
+ Singleton


    
## GUI Component
+ cl_gui_container
+ cl_gui_splitter_container
+ cl_salv_table
+ cl_salv_events_table
+ cl_salv_tree
+ cl_salv_events_tree
+ salv_de_tree_image



## Usage
```
lc_gui_screen=>create_screen( ).
```
