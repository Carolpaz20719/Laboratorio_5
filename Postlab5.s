; Archivo:	Laboratorio_5.s
; Dispositivo:	PIC16F887
; Autor:	Carolina Paz 20719
; Compilador:	pic-as (v2.30), MPLABX V5.40
; 
; Programa:	Contador de 8 bits con dos displays
; Hardware:	Botones y displays
;
; Creado:	20 feb, 2022
; Última modificación: 26 feb, 2022
 

PROCESSOR 16F887
    
; CONFIG1
  CONFIG  FOSC = INTRC_NOCLKOUT ; Oscillator Selection bits (INTOSCIO oscillator: I/O function on RA6/OSC2/CLKOUT pin, I/O function on RA7/OSC1/CLKIN)
  CONFIG  WDTE = OFF            ; Watchdog Timer Enable bit (WDT disabled and can be enabled by SWDTEN bit of the WDTCON register)
  CONFIG  PWRTE = ON            ; Power-up Timer Enable bit (PWRT enabled)
  CONFIG  MCLRE = OFF           ; RE3/MCLR pin function select bit (RE3/MCLR pin function is digital input, MCLR internally tied to VDD)
  CONFIG  CP = OFF              ; Code Protection bit (Program memory code protection is disabled)
  CONFIG  CPD = OFF             ; Data Code Protection bit (Data memory code protection is disabled)
  CONFIG  BOREN = OFF           ; Brown Out Reset Selection bits (BOR disabled)
  CONFIG  IESO = OFF            ; Internal External Switchover bit (Internal/External Switchover mode is disabled)
  CONFIG  FCMEN = OFF           ; Fail-Safe Clock Monitor Enabled bit (Fail-Safe Clock Monitor is disabled)
  CONFIG  LVP = ON              ; Low Voltage Programming Enable bit (RB3/PGM pin has PGM function, low voltage programming enabled)

; CONFIG2
  CONFIG  BOR4V = BOR40V        ; Brown-out Reset Selection bit (Brown-out Reset set to 4.0V)
  CONFIG  WRT = OFF             ; Flash Program Memory Self Write Enable bits (Write protection off)

// config statements should precede project file includes.
#include <xc.inc>

Timer_reset MACRO TMR_VAR
    BANKSEL TMR0	    ; cambiamos de banco
    MOVLW   TMR_VAR	    
    MOVWF   TMR0	    ; configura tiempo de retardo (10ms de retardo)
    BCF	    T0IF	    ; limpiamos bandera de interrupción
    ENDM 
  
UP    EQU 5
DOWN  EQU 7
  
; ------- VARIABLES EN MEMORIA --------
PSECT udata_shr		  ; Memoria compartida
    W_TEMP:		DS 1
    STATUS_TEMP:	DS 1
    
PSECT udata_bank0         ;reservar memoria
    cont:		DS 1
    valor:		DS 1	; Contiene valor a mostrar en los displays de 7-seg
    banderas:		DS 1	; Indica que display hay que encender
    num:                DS 3
  ;  nibbles:		DS 2	; Contiene los nibbles alto y bajo de valor
    display:		DS 3	; Representación de cada nibble en el display de 7-seg
    
    
PSECT resVect, class=CODE, abs, delta=2
ORG 00h	    ; posición 0000h para el reset
;------------ VECTOR RESET --------------
resetVec:
    PAGESEL Main	  ; Cambio de pagina
    GOTO    Main

PSECT intVect, class=CODE, abs, delta=2
ORG 04h			    ; posición 0004h para interrupciones
;------- VECTOR INTERRUPCIONES ----------
    
PUSH:
    MOVWF   W_TEMP	    ; Guardamos W
    SWAPF   STATUS, W
    MOVWF   STATUS_TEMP	    ; Guardamos STATUS
    
ISR:
    BTFSC   RBIF	    ; Verificamos bandera del puertoB
    CALL    INT_ONC	    ; Llamar a su subrutina de interrupción correspondiente
    
    BTFSC   T0IF	    ; Verificamos bandera del TMR0
    CALL    INT_TMR0	    ; Llamar a su subrutina de interrupción correspondiente
         
POP:
    SWAPF   STATUS_TEMP, W  
    MOVWF   STATUS	    ; Recuperamos el valor de reg STATUS
    SWAPF   W_TEMP, F	    
    SWAPF   W_TEMP, W	    ; Recuperamos valor de W
    RETFIE		    ; Regresamos a ciclo principal  
    
PSECT code, delta=2, abs
ORG 100h                  ; posición 100h para el codigo
 
;-------------SUBRUTINAS DE INTERRUPCION--------
INT_ONC:
    BANKSEL PORTB
    BTFSS   PORTB, UP	  ; Ver si botón ya no está presionado
    CALL    INC
    BTFSS   PORTB, DOWN	  ; Ver si botón ya no está presionado
    CALL    DECR
    BCF     RBIF	  ; Limpiar la Bandera del puerto B
    RETURN

INT_TMR0:
    Timer_reset	217	    ; Llamar la macro del Timer_reset
    CALL    MOSTRAR_VALOR   ; Mostramos valor en hexadecimal en los displays
    RETURN
   
INC:
    INCF   cont          ; Incrementar 1 y se almacena en el registro F
    MOVF   cont, 0        ; mover el valor a w
    MOVWF  PORTA 
    RETURN
    
DECR:
    DECF    cont	  ; Decrementar el puertoA
    MOVF    cont, 0       ; mover el valor a w
    MOVWF   PORTA
    RETURN
    
;-----------Tablas---------------------------
ORG 200h
Tabla:
    CLRF    PCLATH		; Limpiamos registro PCLATH
    BSF	    PCLATH, 1		; Posicionamos el PC en dirección 02xxh
    ANDLW   0x0F		; No saltar más del tamaño de la tabla
    ADDWF   PCL
    RETLW   00111111B	;0
    RETLW   00000110B	;1
    RETLW   01011011B	;2
    RETLW   01001111B	;3
    RETLW   01100110B	;4
    RETLW   01101101B	;5
    RETLW   01111101B	;6
    RETLW   00000111B	;7
    RETLW   01111111B	;8
    RETLW   01101111B	;9
    RETLW   01110111B	;A
    RETLW   01111100B	;b
    RETLW   00111001B	;C
    RETLW   01011110B	;d
    RETLW   01111001B	;E
    RETLW   01110001B	;F
    
;------------- CONFIGURACION -----------------
Main:
    CALL    IO_config	   ; Configuración de I/O
    CALL    Reloj_config   ; Configuración de Oscilador
    CALL    Timer0_config  ; Configuración de TMR0
    CALL    ONC_config	   ; Configuración del on-change
    CALL    INT_config	   ; Configuración de las interrupciones
    BANKSEL PORTB
       
;----------------loop principal-----------------
Loop:
    MOVF    PORTA, W		; Valor del PORTA a W
    MOVWF   valor		; Movemos W a variable valor
    CALL    OBTENER_NUM  	; Guardamos nibble alto y bajo de valor
    CALL    SET_DISPLAY		; Guardamos los valores a enviar en PORTC para mostrar valor en hex
    CLRF    num			; Limpiar num
    CLRF    num+1		; Limpiar num+1
    CLRF    num+2		; Limpiar num+2
    goto    Loop	        ; Loop 
    
;------------- SUBRUTINAS ---------------
IO_config:
    BANKSEL ANSEL	  ; Cambiar de Banco
    CLRF    ANSEL
    CLRF    ANSELH	  ; Poner I/O digitales
    
    BANKSEL TRISA
    CLRF    TRISA	  ; PORTC como salida
    BSF     TRISB, UP	  ; Poner como entradas
    BSF     TRISB, DOWN
    CLRF    TRISC	  ; Poner como salida
    BCF	    TRISD, 0	  ; RD0 como salida / display centenas
    BCF	    TRISD, 1    	; RD1 como salida / display decenas
    BCF	    TRISD, 2		; RD2 como salida / display unidades
        
    BCF     OPTION_REG, 7
    BSF     WPUB, UP	  ; Habilitar pull-ups
    BSF     WPUB, DOWN
          
    BANKSEL PORTA	  ; Cambiar de Banco
    CLRF    PORTA	  ; Limpiar PORTA
    CLRF    PORTC	  ; Limpiar PORTC
    CLRF    PORTD	  ; Limpiar PORTC
    CLRF    banderas	  ; Limpiamos banderas
    RETURN
  
Reloj_config:
   BANKSEL  OSCCON        ; cambiamos a banco 1
   BSF	    SCS	          ; SCS =1, Usamos reloj interno
   BSF	    IRCF2         ; IRCF 1
   BSF	    IRCF1         ; IRCF 1
   BCF	    IRCF0         ; IRCF 0 --> 110 4MHz
   RETURN

Timer0_config:
   BANKSEL  OPTION_REG	  ; cambiamos de banco de OPTION_REG
   BCF	    T0CS	  ; Timer0 como temporizador
   BCF	    PSA   	  ; Prescaler a TIMER0
   BSF	    PS2	          ; PS2
   BSF	    PS1	          ; PS1
   BSF	    PS0	          ; PS0 Prescaler de 1 : 256
    
   BANKSEL  TMR0	  ; cambiamos de banco de TMR0
   MOVLW    217		  ; 10ms = 4*1/4MHz*(256-x)(256)
   MOVWF    TMR0	  ; 10ms de retardo
   BCF	    T0IF	  ; limpiamos bandera de interrupción
   RETURN 
      
ONC_config:
    BANKSEL TRISA	  ; Cambiar de banco
    BSF     IOCB, UP	  ; Habilitar INTERRUPT-ON-CHANGE PORTB REGISTER
    BSF     IOCB, DOWN    ; Habilitar INTERRUPT-ON-CHANGE PORTB REGISTER
    
    BANKSEL PORTA	  ; Cambiar de Banco
    MOVF    PORTB, W	  ; Al leer termina condición de mismatch
    BCF     RBIF	  ; Limpiar la bandera de puertoB
    RETURN
   
INT_config:
   BANKSEL INTCON
   BSF	    GIE		  ; Habilitamos interrupciones
   BSF	    RBIE	  ; Habilitamos interrupcion PUERTOB
   BCF	    RBIF	  ; Limpiamos bandera de puertoB
   BSF	    T0IE	  ; Habilitamos interrupcion TMR0
   BCF	    T0IF	  ; Limpiamos bandera de TMR0
   RETURN
 
OBTENER_NUM:
    CENTENAS:		
	MOVLW  100	    ; Mover la literal de 100 a W
	SUBWF  valor, W     ; valor - 100
	BTFSS  STATUS, 0    ; verificar la resta 
        GOTO   DECENAS	    ; Vamos a decenas
	MOVWF  valor	    ; mover valor a w al registro f
	INCF   num	    ; incrementamos el contador de centenas
	GOTO   CENTENAS	    ; regresar a centenas
    
    DECENAS:
	MOVLW  10           ; Mover la literal de 10 a W
	SUBWF  valor, W     ; valor - 10
	BTFSS  STATUS, 0    ; Verificar la resta
        GOTO   UNIDADES	    ; Vamos a unidades
	MOVWF  valor	    ; mover valor a w al registro f
	INCF   num +1 	    ; incrementamos el contador de centenas
	GOTO   DECENAS	    ; regresar a decenas
    
    UNIDADES:
	MOVLW  1            ; Mover la literal de 1 a W
	SUBWF  valor, W     ; valor - 1
	BTFSS  STATUS, 0    ; Verificar la resta
        RETURN
	MOVWF  valor	    ; mover valor a w al registro f
	INCF   num+2	    ; incrementamos el contador de centenas
	GOTO   UNIDADES     ; regresar a unidades 

    
SET_DISPLAY:
    MOVF    num, W		; Movemos nibble bajo a W
    CALL    Tabla		; Buscamos valor a cargar en PORTC
    MOVWF   display		; Guardamos en display
    
    MOVF    num+1, W	        ; Movemos nibble alto a W
    CALL    Tabla		; Buscamos valor a cargar en PORTC
    MOVWF   display+1		; Guardamos en display+1
       
    MOVF    num+2, W	        ; Movemos nibble alto a W
    CALL    Tabla		; Buscamos valor a cargar en PORTC
    MOVWF   display+2		; Guardamos en display+1
    RETURN
    
MOSTRAR_VALOR:
    BCF	    PORTD, 0		; Apagamos display 
    BCF	    PORTD, 1		; Apagamos display 
    BCF	    PORTD, 2		; Apagamos display 
    
    BTFSC   banderas, 1		; Verificamos bandera
    GOTO    DISPLAY_2		; Ir al display2
    BTFSC   banderas, 0		; Verificamos bandera
    GOTO    DISPLAY_1		; Ir al dispaly1
      
    DISPLAY_0:			
	MOVF    display, W	; Movemos display a W
	MOVWF   PORTC		; Movemos Valor de tabla a PORTC
	BSF	PORTD, 0	; Encendemos display de nibble bajo
	BCF	banderas, 1	; Poner banderas en 1
        BSF     banderas, 0	; Poner banderas en 0
    RETURN

    DISPLAY_1:
	MOVF    display+1, W	; Movemos display+1 a W
	MOVWF   PORTC		; Movemos Valor de tabla a PORTC
	BSF	PORTD, 1	; Encendemos display de nibble alto
	BSF	banderas, 1	; Poner banderas en 0
    RETURN
    
    DISPLAY_2:
	MOVF    display+2, W	; Movemos display+2 a W
	MOVWF   PORTC		; Movemos Valor de tabla a PORTC
	BSF	PORTD, 2	; Encendemos display de nibble alto
	BCF	banderas, 1	; Poner banderas en 1
	BCF	banderas, 0	; Poner banderas en 1
    RETURN

    
END
