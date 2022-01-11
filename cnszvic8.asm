			        IDNT CNSZVIC8

			        OPT MUL

				BTEXT
------------------------------------------------------------------------------------------

NOMBRE : CENdec5T.SRC
DESCRIP: Central decodigicadora 5 tonos sistema ZVEI-1 y EEA, ambos con
         cabecera 00 y velocidad variable.
MICRO. : MC68HC705C8/C8
OBSERV.: Controla el FX803 por el SCI.
         Con filtro.

------------------------------------------------------------------------------------------
 				ETEXT

*
* DEFINICION DE LOS POrts

PORTA   equ 00H
PORTB   equ 01H
PORTC   equ 02H
PORTD   equ 03H
PCA     equ 04H
PCB     equ 05H
PCC     equ 06H
PCD     equ 07H
MR      equ 0AH         *MISCELLANEOUS REGISTER
SCDAT   equ 0011H       *Serial Comunications Data Register
SCCR1   equ 000EH       *Serial Comunication Register 1
SCCR2   equ 000FH       *Serial Comunication Register 2
SCSR    equ 0010H       *Serial comunication Status Register
SPCR    equ 000AH       *SPCR (SERIAL PERIPHERAL CONTrol REGISTER)
SPSR    equ 000BH       *Serial Peripheral Status Register
SPDR    equ 000CH       *Serial Peripheral Data I/O Register
TCR     equ 0012H       *Timer Control Register
TSR     equ 0013H       *Timer Status Register
TCRH    equ 0018H       *TIMER COUNT REGISTER (HIGH)
TCRL    equ 0019H       *TIMER COUNT REGISTER (LOW)

BAUD    equ 000DH       *Baud Rate Register
ENTRY   equ 100H
INTEX   equ 07FAH
RESET   equ 7FEH

GENERAL_RESET		equ 01h
WRITE_CONTROL_REG	equ 30h
RD_ST_REG		equ 31h
READ_RX_TONE		equ 32h
WR_NOTONE		equ 33h
WR_GEN1			equ 34h
WR_GEN2			equ 35h
WR_GP			equ 36h

AUDIO_SWITCH		equ 7
GP_TIMER_INT		equ 6
decODER_INT		equ 5
SUMMING_SWITCH          equ 4
HIGH_BAND		equ 00
MID_BAND		equ 04
EXTENDED_BAND		equ 08
DAT_DISP  		equ PORTB
CTR_DISP  		equ PORTC
RS         		equ 7
E          		equ 6
WR         		equ 5
FX803_CS   		equ 3
CTRL_FX803 		equ PORTC
CTRL_PITO  		equ PORTC
PITO       		equ 2
TOTAL_PITOS     	equ 3
CTRL_RADIO 		equ PORTC
PTT        		equ 4

                       bsct
                       dsct

contador rmb 1
numero   rmb 1
buftonr  rmb 10 *BUFFER DE CODIGOS RECIBIDOS (HASTA 2 CODIGOS DE 5 BYTES)
puntbuft rmb 1
tablad   rmb 1
antton   rmb 1  *ULTIMO TONO RECIBIDO
antton1  rmb 1
flag     rmb 1
conta    rmb 1
conta2   rmb 1
sava     rmb 1  *. PARA A EN PRETIM
sava2    rmb 1
savx     rmb 1
temp     rmb 1  *tempORAL PARA LO QUE SEA
tpm      rmb 3
Nh       rmb 1
RhH      rmb 1
RhL      rmb 1
result0  rmb 1
result1  rmb 1
result2  rmb 1  *tempORAL PARA rmbTA
result3  rmb 1
mltndo0  rmb 1  *multiPLICANDO 1
mltndo1  rmb 1  *multiPLICANDO 2
mltdor   rmb 1  *multiPLICADOR
R0       rmb 1
R1       rmb 1
R2       rmb 1
R3       rmb 1
R4       rmb 1
R5       rmb 1
temp1    rmb 1   *USADO POR RUTINA DIVISION, Y rmbTO AL FINAL (BYTE ALTO)
temp2    rmb 1   *rmbTO BYTE BAJO
DVSOR1   rmb 1   *DIVISOR, BYTE ALTO
DVSOR2   rmb 1   *DIVISOR, BYTE BAJO
temp3    rmb 1
temp4    rmb 1
temp5    rmb 4   *
DVSOR0   rmb 1   *DIVISOR BYTE SUPER ALTO
temp0    rmb 1   *rmbTO DIVISION BYTE SUPER ALTO
temp6    rmb 1   *tempORAL DIVISION
CtrlReg	 rmb 1

vTecsavx   	rmb 1
vTecsava   	rmb 1
vTeccar    	rmb 1
vTecAntC 	rmb 1
vTectime	rmb 1
mtx_savx	rmb 1

vDspCondis	rmb 01
vDspDatdis	rmb 01
vDspStatcr    	rmb 01         *// Estado cursor (on/off)
vDspCrdir     	rmb 01         *// Direcci¢n cursor
vDspBuff      	rmb 4
vDspPosX      	rmb 1

tono_buff       rmb 4
vTimeOut        rmb 2

BcdH            rmb 1
BcdL            rmb 1

                                 psct

        lda #$82
        sta $1fdf

*----------------------------- CONFIGURACION PUERTOS I/O -----------------------

        sei
        lda #$0f        *BITS 1,2,3,4,5 ENTRADAS, rmbTO SALIDAS.
        sta PCA         *INICIALIZA ENTADAS Y SALIDAS DEL PORT A
        lda #$0C
        sta PORTA
        clra
        sta PCB         *PORTB ENTRADAS

        lda #$ef
        sta PCC

        ldx #$30

ClrMem:
        clra
        sta 0,x
        incx
        cpx #$ff
        bne ClrMem

*------------------- RUTINAS DE INICIALIZACION Y PUEsta EN MARCHA --------------

        bset 5,TCR      *INTERRUPT OVERFLOW DEL TIMER PERMITIDO.

        bset FX803_CS,CTRL_FX803    *DESACTIVA EL CS (803)

        jsr IniSpi      	*INICIALIZA EL MODO SPI.
        jsr SciOn		*Activa puerto de comunicaciones.
        jsr fDspIni		*INICIALIZA DISPLAY.
        jsr Ini803      	*INICIALIZA EL 803.
        jsr RST         	*SACA EL TITULO.
        bclr PITO,CTRL_PITO

        lda #$ff
        sta PORTA
        lda PORTA
        cmp #$9f
        bne init

*----------------------------- TEST ---------------------------------------------
        clrx
test1:
        lda test,x
        sta vDspDatdis
        jsr fDisp_wr
        incx
        cpx #$10
        bne test1

        lda #$40        *DIRECCION SEGUNDA LINEA DISPLAY.
        sta vDspCondis
        jsr fDisp_CrPos        *
        clrx
test2:
        lda genera,x
        sta vDspDatdis
        jsr fDisp_wr
        incx
        cpx #$10
        bne test2

test3:
        lda #'0'
        sta tono_buff
        sta tono_buff+1

        lda #'0'
        sta tono_buff+2

        lda #'A'
        sta tono_buff+3

        lda #'5'
        sta tono_buff+4
        jsr TxTono
        bclr 4,flag
        bra test3

*---------------------------- CONSOLA -----------------------------------------

init:
	jsr fDisp_ClrDsp
        jsr SACAREC     	*FORMATO PANTALLA.

        lda #$03
        sta conta

        lda #$FF
        sta antton

        lda #$3
        sta contador

        CLR puntbuft
        CLR tablad
        CLR flag
*--------------------------------------- MAIN PROGRAM --------------------------
        cli

main:
        jsr getch       *Esperamos una pulsaci¢n del teclado
        beq main

ModoTx:
        bset 4,flag     *No decodifica

        sta sava2

        lda #'0'
        sta vDspBuff
        lda #'0'
        sta vDspBuff+1
        lda #'0'
        sta vDspBuff+2

        sei
	bclr FX803_CS,CTRL_FX803
	lda #WR_GP
	jsr SpiOut
        lda #$0f
	jsr SpiOut
	bset FX803_CS,CTRL_FX803

	bclr FX803_CS,CTRL_FX803
	lda #WRITE_CONTrol_REG
	jsr SpiOut
        bset GP_TIMER_INT,CtrlReg
	lda CtrlReg
	jsr SpiOut
	bset FX803_CS,CTRL_FX803
        cli

        clra
        sta vDspCondis
        jsr fDisp_CrPos
        clrx

        *'Entre indicativo'
tit1:
        lda indic,x
        sta vDspDatdis
        jsr fDisp_wr
        incx
        cpx #$10
        bne tit1

        lda #$40        *DIRECCION SEGUNDA LINEA DISPLAY.
        sta vDspCondis
        jsr fDisp_CrPos        *
        clrx

     *'      -   -      '
tit2:
        lda input,x
        sta vDspDatdis
        jsr fDisp_wr
        incx
        cpx #$10
        bne tit2

        lda #$47
        sta vDspCondis
        jsr fDisp_CrPos     *Posiciona el cursor
        jsr fdisp_CrOn      *Activa el cursor

        clr vDspPosX          *N£mero de caractermb entrados
        lda sava2
wTec:

        ldx #$ff
        stx vTimeOut
        ldx #$ff
        stx vTimeOut+1
        bclr 1,flag

        ldx vDspPosX
        cpx #$3
        bne slt2
        jsr PitoMal
        bra wTe

slt2:
        sta vDspDatdis
        cmp #'0'
        bmi wTe
        cmp #'A'
        bpl wTe

        sta sava

        lda vDspBuff
        sta tpm
        lda vDspBuff+1
        sta tpm+1
        lda vDspBuff+2
        sta tpm+2

        ldx vDspPosX
        lda sava
        sta tpm,x

        clra
        ora tpm
        sub #'0'
        sta BcdH

        lda tpm+1
        sub #'0'
        lsla
        lsla
        lsla
        lsla
        and #$f0
        sta BcdL

        lda tpm+2
        sub #'0'
        and #$0f
        ora BcdL
        sta BcdL

	BTEXT
        lda BcdH
        cmp limite
        beq SigSac2
        bpl wTe

SigSac2:
        lda BcdL
        cmp limite+1
        beq TecOk
        bpl wTe
        ETEXT

TecOk:
        lda sava
        jsr fDisp_wr         *Lo ponemos en el display

        lda vDspDatdis
        sta vDspBuff,x
        inc vDspPosX

wTe:
        brclr 1,flag,NoTimeOut

        dec vTimeOut
        bne NoTimeOut
        dec vTimeOut+1
        bne NoTimeOut
        jmp BorrOk

NoTimeOut:

        cmp #'A'                *Llamada de grupo
        beq grupo

        jsr HayPtt              *Mira si hay ptt
        bne PttOn
        jsr getch               *Esperamos una tecla
        beq wTe
        cmp #'B'
        beq borrar             *Comando para borrar
        jmp wTec

grupo:
        lda vDspPosX
        cmp #00
        bne NoGrp

        lda #'G'
        sta vDspDatdis
        jsr fDisp_wr
        lda #'R'
        sta vDspDatdis
        jsr fDisp_wr
        lda #'P'
        sta vDspDatdis
        jsr fDisp_wr
        lda #$3
        sta vDspPosX

        lda #'A'
        sta vDspBuff
        sta vDspBuff+1
        sta vDspBuff+2
        clra
        bra wTe

NoGrp:
        jsr PitoMal
        bra wTe

borrar:
        ldx vDspPosX
        cpx #00
        beq BorrOk              *Si han pulsado alg£n car cter empieza a
        jmp ModoTx              *editar de nuevo

BorrOk  jsr Ini803              *Si no han pulsado nung£n car cter, regrmba
        jsr fDisp_CursorOff     *a main.
        bclr 4,flag
        jmp init

PttOn:
        lda vDspPosX
        cmp #$3
        beq TxIndic
        jsr PitoMal
esp:
        jsr HayPtt      *Espera que suelten el PTT
        bne esp
        bra wTe

*------------------------- transmite indicativo -----------------------------
*
TxIndic:

        jsr fDisp_ClrDsp
        clrx

       *'-->  llama <--'
tit3:
        lda llama,x
        sta vDspDatdis
        jsr fDisp_Wr
        incx
        cpx #$10
        bne tit3

        jsr BucleTx

        lda #'0'
        sta tono_buff
        sta tono_buff+1

        lda vDspBuff
        sta tono_buff+2

        lda vDspBuff+1
        sta tono_buff+3

        lda vDspBuff+2
        sta tono_buff+4
        jsr TxTono

        lda #'0'
        sta tono_buff
        sta tono_buff+1

        lda ident
        sta tono_buff+2

        lda ident+1
        sta tono_buff+3

        lda ident+2
        sta tono_buff+4
        jsr TxTono
        bclr 4,flag
        jmp init

*----------------------------------- subRUTINAS --------------------------------

TxTono:

        sei

	bclr FX803_CS,CTRL_FX803
	lda #WR_GP
	jsr SpiOut
        lda #$05         *Programa 70 ms para timer
	jsr SpiOut
	bset FX803_CS,CTRL_FX803

	bclr FX803_CS,CTRL_FX803
	lda #WRITE_CONTrol_REG
	jsr SpiOut
        bset GP_TIMER_INT,CtrlReg
        bclr AUDIO_SWITCH,CtrlReg
        bset SUMMING_SWITCH,CtrlReg
	lda CtrlReg
	jsr SpiOut
	bset FX803_CS,CTRL_FX803
        cli

        bclr 1,flag
tTimer:
        brclr 1,flag,tTimer    *Espera Time Out

tst:
	clrx
        clr temp2

TxIndBuc:
        lda tono_buff,x
        cmp temp2
        bne NoR

        lda #$0b
        sta temp2
        bra NoRes
NoR:
        cmp #'A'
        bne NoRs

        sta temp2
        lda #$0a
        bra NoRes
NoRs:
        sta temp2
	sub #'0'

NoRes   stx savx
	ldx #2
        mul
	tax

	lda EEA_TX,x
	sta temp
	incx
	lda EEA_TX,x
	sta temp1

	bclr FX803_CS,CTRL_FX803
	lda #WR_GEN1
	jsr SpiOut
	lda temp
	jsr SpiOut
	lda temp1
	jsr SpiOut
	bset FX803_CS,CTRL_FX803

	bclr 1,flag
wtTimer:
        brclr 1,flag,wtTimer    *Espera Time Out

	ldx savx
        incx
        cpx #$5
	bne TxIndBuc

FinTxIndic:
        jsr Ini803
        jsr fDisp_CursorOff
        rts

*----------------------------------------------------------------------------

*Mira si hay una pulsaci¢n segura de PTT
*Si la hay regresa con el acumulador a 1
*Si no la hay regresa con el acumulador a 0

HayPtt:
        brclr PTT,CTRL_RADIO,NoPtt      *No estÿ pulsado

        ldx #10
rebotes:
        jsr AntiReb
        brclr PTT,CTRL_RADIO,NoPtt
        decx
        bne rebotes

        lda #1
        rts
NoPtt:
        clra
        rts

*------------------------------ CALCULA LA RESTA 511-Rh ------------------------
FORMULA:

*EL numero A RESTAR HA DE VENIR EN RhL

        lda #$FF
        sub RhL
        sta RhL
        lda #$1
        sta RhH

*RESULTADO =
*       RhH BYTE ALTO (SIEMPRE 1)
*       Rhl BYTE BAJO

*-------------------------------------------------------------------------------

*-------------------------------- multiPLICA 28000 x Nh ------------------------

        lda #$6D
        sta mltndo0
        lda #$60
        sta mltndo1             *6D60H = 28000d
        lda Nh
        sta mltdor
        jsr multi               *multiPLICO 28000 x Nh

*-------------------------------------------------------------------------------

*------------------- DIVIDE (28000xNh) / (511-Rh) ------------------------------

        lda result2             *EL RESULTADO QUEDA EN result0, result1, result2
        sta R3
        lda result1
        sta R0
        lda result0
        sta R1

        lda #$00
        sta DVSOR0
        lda RhH
        sta DVSOR1
        lda RhL
        sta DVSOR2
        jsr DIV

        lda R3
        AND #$F0
        rora
        rora
        rora
        rora
        sta temp

        lda R0
        rola
        rola
        rola
        rola
        ORA temp
        sta R0
        lda R3
        AND #$0F        *multiPLICA POR 10 EL RESULTADO DE LA FORMULA PARA DAR
        sta R3          *MAS PRECISION.
        rts

*INICIALIZA EL contador DEL TIMER

INITIM:
        lda #$FE
        sta TCRH
        lda #$FF
        sta TCRL
        rts

*---------------------------------- getch --------------------------------------
*Obtiene una tecla del teclado, el teclado se compone de todos los pulsadormb
*del panel frontal.
*Mientras se hacen pulsaciones cortas en cualquier tecla, la rutina getch,
*devuelve el byte 'car' el caracter pulsado, si no se pulsa ninguna tecla
*devuelve un 00.
*
*Si el caracter se mantiene pulsado, hace un bucle hasta que se suelta
*la tecla, si no se suelta ira haciendo bucles m s cortos.
*entre bucle y bucle va devolviendo la tecla pulsada.
*
*Para saber si la pulsaci¢n de la tecla es la primera, hace una comparaci¢n
*del byte 'car' con el byte 'Antcar', el byte Antcar contiene la oltima tecla
*pulsada.
*Si son diferentes, no har  ningon bucle, pero si son iguales hara el bucle.
*una vez acabado el bucle, cambia el byte time, que inicialmente esta cargado
*con #$f0, a #$30, para que el pr¢ximo bucle sea m s corto, ya que el byte
*time indica la duraci¢n del bucle.
*
*El conmutador es independiente del teclado, en la variable 'AntConmut', se guar
*la posici¢n del conmutador, mientras sea igual la rutina getch lo ignora hasta
*que haya alg£n cambio en el conmutador.
*
getch:
        jsr LeeMatriz
        tsta
        beq FinGetc3            *// Si no es as¡, regrmba con un 00.

        sta vTeccar
        jsr AntiReb             *// Bucle antirrebotes
        jsr LeeMatriz
        cmp vTeccar            *// Mira si la tecla pulsada es la misma que
        bne FinGetc3            *// antes. Si no es asð, regrmba con un 00.

        cmp vTecAntC         *// Mira si la tecla de antes aon sigue pulsada.
        bne FinGetc2            *// si no es asð, regrmba con la tecla recien
                                *// pulsada.
GetBu:
        ldx vTectime           *// Inicia el bucle con la duraci½n indicada
                                *// en time.
GetBuc:
        jsr AntiReb             *// Bucle antirrebotes
        jsr LeeMatriz
        cmp vTeccar            *// Mira si aon sigue la tecla pulsada.
        bne FinGetc3            *// Han soltado la tecla, regrmba con un 00.
        decx
        cpx #$00                *// Fin bucle?
        bne GetBuc              *// No, continua ...

        lda #$40                *// Como ya ha hecho un bucle largo, los (80)
        sta vTectime           *// siguientes han de ser cortos.

FinGetc2:
        jsr PitoOk
        lda vTeccar            *// Memoriza LA TECLA RECIEN PULSADA.
        sta vTecAntC
        rts

FinGetc3:
        lda #$80                *// El siguiente bucle tendra que ser largo.
        sta vTectime
        clr vTeccar            *// Regrmba con un 00.
        clr vTecAntC         *// Borra la tecla memorizada.
        rts

LeeMatriz:

        lda #$1
        sta vTecsava
matriz:
        lda vTecsava
        sta PORTA
        lda PORTA
        and #$f0
        bne HayTecla
        clc
        rol vTecsava
        lda vTecsava
        cmp #$10
        bne matriz
        clra
        rts

HayTecla:

        stx mtx_savx
        clrx
        lda PORTA
        sta sava
FindTab:

        lda TablaTeclado,x
        cmp sava
        beq found
        incx
        incx
        cpx #32
        bmi FindTab
        ldx mtx_savx
        clra
        rts

found:
        incx
        lda TablaTeclado,x
        ldx mtx_savx
        rts

*--------------------------------------------------------------------------

PitoOk:
        bset PITO,CTRL_PITO
        jsr bucle
        bclr PITO,CTRL_PITO
        rts

PitoMal:
        bset PITO,CTRL_PITO
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        jsr bucle
        bclr PITO,CTRL_PITO
        rts

bucle:
        ldx #$10
buc1    lda #$ff
buc:
        deca
        bne buc
        decx
        cpx #$00
        bne buc1
        rts

BucleTx:
        lda #$2f
BucleT:
        sta sava
        jsr bucle
        lda sava
        deca
        bne BucleT
        rts

* ---------------------------------------- DIV ---------------------------------
*subRUTINA PARA DIVIDIR.
*EN DVSOR1 (BYTE ALTO) Y DIVSOR2 (BAJO) VA EL DIVISOR
*EN R1 (BYTE ALTO) Y R0 (BAJO) EL DIVIDENDO
*EL rmbULTADO QUEDA EN R1 Y R0

DIV:
        com DVSOR0
        com DVSOR1
        com DVSOR2      *comPLEMENTA DIVISOR
        CLC
        lda #$1         *INCREM.DIVISOR PARA DOS comPLEM.
        adc DVSOR2
        sta DVSOR2
        clra
        adc DVSOR1
        sta DVSOR1
        clra
        adc DVSOR0
        sta DVSOR0
        CLR temp0
        CLR temp1       *VALOR INICIAL PARA rmbTO
        CLR temp2
        LDX #25         *contador
DV0:
        CLC             *SUMA DVSOR1-2 A temp1-2
        lda temp2
        adc DVSOR2
        sta temp4
*
        lda temp1
        adc DVSOR1
        sta temp3
        lda temp0
        adc DVSOR0
        sta temp5
*
        BCC DV1
        lda temp4
        sta temp6
        lda temp2
        sta temp4
        lda temp6
        sta temp2
        lda temp3
        sta temp6
        lda temp1
        sta temp3
        lda temp6
        sta temp1
        lda temp5
        sta temp6
        lda temp0
        sta temp5
        lda temp6
        sta temp0

DV1:
        rol R3
        rol R0          *ROTA BYTE BAJO DIVIDENDO
        rol R1
        rol temp2
        rol temp1
        rol temp0
        decX
        bne DV0
*                       TRAS LA DIVISION ENTERA, ROTA rmbTO Y LO DEJA EN temp1-2
        ror temp0
        ror temp1
        ror temp2
        rts
*
*-------------------------------------------------------------------------------

*------------------------------------------ multi-------------------------------
* EN mltndo0 BYTE ALTO multiPLICADOR
* EN mltndo1 BYTE BAJO multiPLICADOR
* EN mltdor multiPLICANDO.
* result0, result1, result2 rmbULTADO

multi:
        CLR result0
        CLR result1
        CLR result2

        lda mltndo1
        LDX mltdor

        mul         *MUL

        STX result1
        sta result2     *rmbULTADO BYTE BAJO FINAL

        STX sava

        lda mltndo0
        LDX mltdor
        mul         *MUL
        sta result1
        CLC
        lda sava
        ADD result1
        BCS HayCarry

        sta result1
        STX result0
        rts

HayCarry:

        INC result0
        LDX mltdor
        mul         *MUL
        sta result1

        TXA
        ADD result0
        LDX mltdor
        mul         *MUL

        sta result0
        rts
*-------------------------------------------------------------------------------

*------------------------------------ subRUTINAS GESTION 803 -------------------

*- - - - - - - - - - - - - - - - - - - - INICIALIZA EL 803 - - - - - - - - - - -
Ini803:

*PRIMERO HACE UN RESET DEL 803
*
        bclr FX803_CS,CTRL_FX803
        lda #GENERAL_rmbET
        jsr SpiOut
        bset FX803_CS,CTRL_FX803

*TRANSMITE UN 00 AL CONTrol REGISTER
*
*  BITS                 SIGNIFICADO
*
*   7                AUDI SWITCH          0 = DISABLE
*   6                G/PURPOSE TIMER      0 = DISABLE
*   5                decODER INTERRUPTS   0 = DISABLE
*   4                SUMMING SWITCH       1 = ENABLE
*   3-2              BAND SELECTION     0 0 = HIGH BAND
*   1-0              NO SE USA          0 0
*
        sei
        bclr FX803_CS,CTRL_FX803    *CHIP SELECT
        lda #WRITE_CONTrol_REG      *DIRECCION CONTrol REGISTER
        jsr SpiOut
        lda #$80                    *INSTRUCCION CONTrol REGISTER
        sta CtrlReg
        jsr SpiOut
        bset FX803_CS,CTRL_FX803

* DESACTIVA EL TRANSMISOR

        bclr FX803_CS,CTRL_FX803    *CHIP SELECT
        lda #WR_GEN1
        jsr SpiOut
        lda #$40
        jsr SpiOut
        lda #$00
        jsr SpiOut
        bset FX803_CS,CTRL_FX803    *CHIP SELECT

        bclr FX803_CS,CTRL_FX803    *CHIP SELECT
        lda #WR_GEN2
        jsr SpiOut
        lda #40
        jsr SpiOut
        lda #00
        jsr SpiOut
        bset FX803_CS,CTRL_FX803    *CHIP SELECT

*PROGRAMA EL NOTONE REGISTER CON 40 ms

        bclr FX803_CS,CTRL_FX803    *CHIP SELECT
        lda #WR_NOTONE     *DIRECCION NO TONE REGISTER
        jsr SpiOut
        lda #$02
        jsr SpiOut      *TIEMPO MAXIMO
        bset FX803_CS,CTRL_FX803

        bclr FX803_CS,CTRL_FX803    *CHIP SELECT
        lda #WRITE_CONTrol_REG
        jsr SpiOut
	bset decODER_INT,CtrlReg    *Permite interrupts del decodificador
	lda CtrlReg
        jsr SpiOut
        bset FX803_CS,CTRL_FX803
        cli
        rts

*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*- - - - - - - - TRANSMITE AL SPI EL CONTENIDO DE A - - - - - - - - - - - - -
SpiOut:
        sta SPDR
EEW4    brclr 7,SPSR,EEW4         *ESPERA QUE TRANSMITA EL BYTE
        rts
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*-------------- TRANSMITE AL PC EL CONTENIDO DEL REGISTRO A ------------------

SciOut:
	brclr 7,SCSR,SciOut	  *MIRA SI ESTA OCUPADO
	sta SCDAT
ESPERA:
	brclr 6,SCSR,ESPERA	  *ESPERA PARA ACABAR DE TRANSMITIR
        rts
*-----------------------------------------------------------------------------

RdStatus:
        bclr FX803_CS,CTRL_FX803
        lda #RD_ST_REG        *LEERA EL staTUS REGISTER
        jsr SpiOut
        jsr SpiIn       *EN A Esta EL CONTENIDO DEL staTUS REGISTER
        bset FX803_CS,CTRL_FX803
        sta temp
        rts

*- - - - - - - - - LEE UN BYTE DEL SPI Y LO PONE EN A - - - - - - - - - - - -
SpiIn:
*        lda #$53         *CAMBIA LA FASE DE CLOCK
*        sta SPCR
        clra            *TRANSMITE UN 00 PARA QUE GENERE EL CLOCK.
        jsr SpiOut      *Y ASI PERMITIR AL DEVICE QUE TRANSMITA EL BYTE.
*        lda #$53        *rmbTAURA LA FASE DE CLOCK.
*        sta SPCR
        lda SPDR        *LEE EL BYTE RECIBIDO.
        rts
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*-----------------------------------------------------------------------------

AntiReb:
        sta sava
        STX savx
        lda #$05
anti2:
        LDX #$FF
anti3:
        decX
        bne anti3
        decA
        bne anti2
        LDX savx
        lda sava
        rts
*-----------------------------------------------------------------------------

*=============================================================================

*---------------------------- INICIALIZACION SPI -----------------------------

***************************** IMPORTANTE *************************************
*                                                      __
*PARA QUE EL MICRO ACEPTE SER MASTER SE HA DE PONER EL SS A POSITIVO (PATA 37),
*SI NO ES ASI, EL MICRO RECHAZA EL BIT 4 DEL SPCR (MASTER).
*
******************************************************************************

IniSpi:
        lda #$53        *Serial Peripheral Interrupt Disable.
        sta SPCR        *Serial Peripheral System Enable.
                        *Master mode
                        *SCK line idles in low state
                        *     __
                        *When SS is low, first edge of SCK invokes first data
                        *sample.
                        *Internal Processor Clock Divided by 32

        rts

*------------------------------------------------------------------------------

*----------------- CONFIGURACION INTERFACE DE COMUNICACIONES ------------------
SciOn:
        LDA #$00
        STA SCCR1

        LDA #$0C        *- TDRE interrupt disabled
                        *- TC interrupt disabled
                        *- SCI interrupt disabled
                        *- IDLE interrupt disbaled
                        *- After the last byte is transmitted, the TDO line
                        *  becomes a hight-impedance line.
                        *- Receiver disabled and RDRF, IDLE, OR, NF and FE
                        *  status bits are inhibited.
        STA SCCR2
	lda #$33
        STA BAUD
        RTS
*------------------------------------------------------------------------------

LOOP:
        STX savx
        LDX #$FF

LO1:
        decX
        CPX #$00
        bne LO1
LO2:
        decX
        CPX #$00
        bne LO2
        LDX savx
        rts

tempB:
        LDX #$FF
tmp:
        jsr LOOP
        decX
        bne tmp
        rts

*--------------------------------- subrutinas gestion display ----------------------------------

*       inicializacion display

fDspIni:

        lda #$3c
        sta vDspCondis
        jsr fDisp_wrcon
        lda #$0c        *//inicia con cursor off
        sta vDspCondis
        sta vDspStatcr
        jsr fDisp_wrcon
        lda #$01
        sta vDspCondis
        jsr fDisp_wrcon

fDisp_ClrDsp:

        lda #$01        *//clear display
        sta vDspCondis
        jsr fDisp_wrcon
        rts

fdisp_CrOn:

        lda #$0e
        sta vDspCondis
        jsr fDisp_WrCon
        rts

fDisp_CursorOff:

        lda #$0c
        sta vDspCondis
        jsr fDisp_WrCon
        rts

fDisp_cradd:

        clr DAT_DISP+4
        bset E,CTR_DISP
        bset WR,CTR_DISP
        bclr RS,CTR_DISP
        lda DAT_DISP
        and #$7f
        rts

fDisp_wrcon:

        jsr fDisp_busy

        bset E,CTR_DISP    *//sube e
        bclr WR,CTR_DISP    *//baja wr
        bclr RS,CTR_DISP    *//baja rs
        lda #$ff
        sta DAT_DISP+4
        lda vDspCondis
        sta DAT_DISP
        bclr E,CTR_DISP    *//   ""      e
        bset WR,CTR_DISP    *//   ""      wr
        bset RS,CTR_DISP    *//rmbtablece rs
        clr DAT_DISP
        rts

* Polsiciona el cursor donde indique condis

fDisp_CrPos:

        lda vDspCondis
        ora #$80
        sta vDspCondis
        jsr fDisp_wrcon
        bclr 7,vDspCondis
        rts

fDisp_busy:

        clra            *//portb input
        sta DAT_DISP+4
        bclr RS,CTR_DISP
        bset WR,CTR_DISP
        bset E,CTR_DISP
busy1:
        brset 7,DAT_DISP,busy1
        rts

fDisp_Wr:

        jsr fDisp_busy

        bset RS,CTR_DISP
        bclr WR,CTR_DISP
        bset E,CTR_DISP
        lda #$ff
        sta DAT_DISP+4
        lda vDspDatdis
        sta DAT_DISP
        bclr E,CTR_DISP
        bset WR,CTR_DISP
        bclr RS,CTR_DISP
        clra
        sta DAT_DISP
        rts

* Saca al display un string de tipo ASCIIZ en la posici½n actual del cursor.
* x ha de venir con la direccion del string .

fDisp_OutStr:

        lda 0,x
        beq FinOut
        sta vDspDatdis
        jsr fDisp_Wr
        incx
        bra fDisp_OutStr
FinOut:
        rts

*----------------- SACA AL DISPLAY EL BUFFER DE TONOS ------------------------

*EN LA LINA DE ABAJO Esta EL CODIGO RECIEN RECIBIDO.

FinSac:
        rts

SACAREC:
	lda #' '
	jsr SciOut	*Lo envía por el puerot serie
*        brset 4,flag,FinSac
        clrx
SacaR:
        lda buftonr,x
        cmp #$0a
        bne NoA

        decx
        lda buftonr,x
        incx
        sta buftonr,x

NoA     incx
        cpx #$5
        bne SacaR

	BTEXT
        lda buftonr+2
        add #'0'
        cmp ident
        bne NoIdent

        lda buftonr+3
        add #'0'
        cmp ident+1
        bne NoIdent

        lda buftonr+4
        add #'0'
        cmp ident+2
        bne NoIdent

        lda #TOTAL_PITOS
        sta conta2      *numero DE PARPADEOS
        bset 3,flag
        bra FinSac
        ETEXT

NoIdent:

        clra
        ora buftonr+2
        sta BcdH

        lda buftonr+3
        lsla
        lsla
        lsla
        lsla
        and #$f0
        sta BcdL
        lda buftonr+4
        and #$0f
        ora BcdL
        sta BcdL

	BTEXT
        lda BcdH
        cmp limite
        beq SigSac
        bpl FinSac

SigSac:
        lda BcdL
        cmp limite+1
        beq CntSac
        bpl FinSac
        ETEXT

CntSac:
        sei
        lda #$44        *DIRECCION SEGUNDA LINEA DISPLAY.
        sta vDspCondis
        jsr fDisp_CrPos        *SITUA EL CURSOR.
        lda #'-'        *PONE UN '-' A CADA EXTREMO DEL CODIGO.
        sta vDspDatdis
        jsr fDisp_Wr

        lda #$4A
        sta vDspCondis
        jsr fDisp_CrPos
        lda #'-'
        sta vDspDatdis
        jsr fDisp_Wr
        lda #$45
        sta vDspCondis
        jsr fDisp_CrPos
        CLRX
SCARE:
        lda buftonr,X
        cmp #$0A
        bne SCAR

        lda temp
        sta buftonr,X
        bra SCA
SCAR:
        sta temp
SCA     ADD #$30
        jsr SciOut	*Lo envía por el puerto serie.
        sta vDspDatdis
        jsr fDisp_Wr
        incx
        CPX #$5
        bne SCARE

*EN LA LIENA DE ARRIBA ESTÁ EL CODIGO RECIBIDO ANTERIORMENTE.

SACANT:
        lda #$05
        sta vDspCondis      *DIRECCION PRIMERA LINEA DISPLAY.
        jsr fDisp_CrPos        *SITUA EL CURSOR
        LDX #$5
SCAN:
        lda buftonr,X
        ADD #$30
        sta vDspDatdis
        jsr fDisp_Wr
        incx
        CPX #$A
        bne SCAN

        CLRX
        lda #$5
        sta temp

*MEMORIZA EL CODIGO RECIEN RECIBIDO.
PASBF:
        lda buftonr,X
        STX savx
        LDX temp
        sta buftonr,X
        INC temp
        LDX savx

        incx
        CPX #$5
        bne PASBF

        lda #TOTAL_PITOS
        sta conta2      *numero DE PARPADEOS
        bset 2,flag     *PERMITA PARPADEO
        rts

*-------------------------- SACA EL TITULO ------------------------------

RST:
        lda #$00
        sta vDspCondis
        jsr fDisp_CrPos
        CLRX
CICLO:
        lda LINEA,X
        sta vDspDatdis
        jsr fDisp_Wr
        incx
        CPX #$10
        bne CICLO

        lda #$40
        sta vDspCondis
        jsr fDisp_CrPos
        CLRX
CICLO2:
        lda LINEA1,X
        sta vDspDatdis
        jsr fDisp_Wr
        incx
        CPX #$10
        bne CICLO2

        LDX #$FF
LP:
        jsr LOOP
        jsr LOOP
        decX
        bne LP
        jsr fDisp_ClrDsp
        rts

*========================= INTERRUPCIONES ==================================
IRQ:
        jsr RdStatus            *MIRA EL EstaDO DEL 803, EL staTUS Esta EN temp
        brset 0,temp,TonVal
        brset 1,temp,NOT
        brset 2,temp,GpTimer

NOT:
        jmp RECHAZ

GpTimer:
	bset 1,flag
	rti

*HA RECIBIDO UN TONO VALIDO.

TonVal:

*        lda puntbuft            *SI ES EL PRIMER INT. LEE INMEDIATO
*        beq TONV

        dec contador            *LEERA CADA 2 INTERRUPTS
        beq TONV
        rti

TONV:
        lda #$3
        sta contador

        bclr FX803_CS,CTRL_FX803
        lda #READ_RX_TONE	*PREGUNTA AL 803 QUE TONO HA RECIBIDO.
        jsr SpiOut
        jsr SpiIn
        sta Nh
        jsr SpiIn
        sta RhL
        bset FX803_CS,CTRL_FX803

        jsr FORMULA             *APLICA LA FORMULA CON EL VALOR LEIDO.
        bclr 0,flag

BS:
        ldx tablad
BSC:
        lda EEA_RX,X
        beq NOEXIS      *ESTE TONO NO EXISTE EN LA TABLA.

        cmp R0
        beq ESNUM
        lda R0
        sub #$2
        cmp EEA_RX,X
        bcc SBSC
        ADD #$4
        cmp EEA_RX,X
        bpl ESNUM
SBSC    incx
        bra BSC

NOEXIS:
        lda puntbuft
        bne FINNOT2
        brset 0,flag,RECHAZ

        lda #$0C        *DIRECCIONA LA TABLA ZVI.
        sta tablad
        bset 0,flag
        bra BS

ESNUM:
        txa
        cmp antton
        beq FINNOT2     *SI ES EL MISMO QUE ANTES, LO RECHAZA.

        sta antton
        cmp #$0C
        bmi ESMEN
        sub #$0C
ESMEN:
*        jsr SciOut	*Lo envía por el puerto serie.
        TAX
        lda puntbuft
        bne NOES0
        CPX #$01
        bne RECHAZ      *SI EL PRIMER DIGITO NO ES UN 01, RECHAZA
        bra ESS3

NOES0:
        lda puntbuft
        cmp #$01
        bne ESS2
        cpx #$08
        bne RECHAZ      *SI EL SEGUNDO DIGITO NO ES UN 08 RECHAZA.
        bra ESS3
ESS2:
        lda puntbuft
        cmp #$02
        bne ESS3
*        TXA
*        cmp #$5
*        bpl RECHAZ      *SI ES >=5 RECHAZA
ESS3:
        TXA
ESS02   LDX puntbuft
        sta buftonr,x

        inc puntbuft            *INCREMENTA EL PUNTERO DEL BUFFER.
        lda puntbuft
        cmp #$05
        bne FINNOT2

        clra
        sta buftonr             *No queremos que la cabecera se 18
        sta buftonr+1           *Así que ponemos 00

        jsr SACAREC
RECHAZ:
        lda #$FF
        sta antton

        CLR puntbuft
        CLR tablad
FINNOT2:
        rti

JtonVAL:
        jmp TonVal

*------------------------------------------------- TIMER ------------------------------------------------

TIMER:
        lda conta2
        bne TIM2
        bclr 2,flag
        bclr 3,flag
TIM2:
        dec conta
        bne CNTIM

        brclr 2,flag,rmbTIM

PARPADEO:
        brset 1,flag,GUION

        brclr 3,flag,NoPito
        bset PITO,CTRL_PITO

NoPito:
        lda #$44        *DIRECCION SEGUNDA LINEA DISPLAY.
        sta vDspCondis
        jsr fDisp_CrPos        *SITUA EL CURSOR.
        lda #' '
        sta vDspDatdis
        jsr fDisp_Wr

        lda #$4A        *DIRECCION SEGUNDA LINEA DISPLAY.
        sta vDspCondis
        jsr fDisp_CrPos        *SITUA EL CURSOR.
        lda #' '
        sta vDspDatdis
        jsr fDisp_Wr
        bset 1,flag
        bra rmbTIM
GUION:
        bclr PITO,CTRL_PITO

        lda #$44        *DIRECCION SEGUNDA LINEA DISPLAY.
        sta vDspCondis
        jsr fDisp_CrPos        *SITUA EL CURSOR.
        lda #'-'        *PONE UN '-' A CADA EXTREMO DEL CODIGO.
        sta vDspDatdis
        jsr fDisp_Wr

        lda #$4A
        sta vDspCondis
        jsr fDisp_CrPos
        lda #'-'
        sta vDspDatdis
        jsr fDisp_Wr
        bclr 1,flag
        dec conta2
        bra rmbTIM

rmbTIM:
        lda #$03
        sta conta
CNTIM:
        jsr INITIM              *rmbTAURA EL contador
        bclr 5,TSR              *DESACTIVA EL INTERRUPT DEL TIMER
        rti

SPI:
SCI:
SWI:
*=============================================================================

*=========================  TABLAS Y MENSAJES ================================

TablaTeclado:
        fcb 41h,'0'
        fcb 88h,'1'
        fcb 48h,'2'
        fcb 28h,'3'
        fcb 84h,'4'
        fcb 44h,'5'
        fcb 24h,'6'
        fcb 82h,'7'
        fcb 42h,'8'
        fcb 22h,'9'
        fcb 81h,'A'
        fcb 21h,'B'
        fcb 11h,'C'
        fcb 12h,'D'
        fcb 14h,'E'
        fcb 18h,'F'

EEA_TX:
	fcb 001h,0fch	*'0'
	fcb 003h,080h	*'1'
	fcb 003h,04ah	*'2'
	fcb 003h,016h	*'3'
	fcb 002h,0e6h	*'4'
	fcb 002h,0b9h	*'5'
	fcb 002h,08eh	*'6'
	fcb 002h,066h	*'7'
	fcb 002h,040h	*'8'
	fcb 002h,01dh	*'9'
        fcb 003h,0bbh	*'G'
        fcb 001h,0ddh	*'R'

EEA_RX:
       fcb 07BH
       fcb 046H
       fcb 04AH
       fcb 04FH
       fcb 054H
       fcb 05aH
       fcb 060H
       fcb 066H
       fcb 06CH
       fcb 074H
       fcb 083H
       fcb 00

ZVI:
        fcb 095H	*0c	0
        fcb 042H 	*0d	1
        fcb 048H 	*0e	2
        fcb 04FH 	*0f	3
        fcb 057H 	*10	4
        fcb 05FH 	*11	5
        fcb 068H 	*12	6
        fcb 072H 	*13	7
        fcb 07CH 	*14	8
        fcb 089H 	*15	9
        fcb 0A2H 	*16	R
        fcb 00
LINEA:
        fcc 16,  ELECTRONICA
LINEA1:
        fcc 16, BARCELONA S.L.
indic:
        fcc 16,Entre indicativo
llama:
        fcc 16,--> llama <--
input:
        fcc 16,      -   -
test:
        fcc 16,      TEST

genera:
        fcc 13,genera 0RG5

ident   fcc 3,300
limite  fcb 07h,25h

*===============================================================================

 				asct
*------------------- INICIALIZACION DE LOS VECTOrmb ----------------------
                               org 1ff0h

        fdb 0000

                               ORG 1FF4H

        fdb SPI
        fdb SCI
        fdb TIMER
        fdb IRQ
        fdb SWI
        fdb ENTRY

                                org 1fdfh       'option register

        fcb 82h               *ram0 = 0
                              *ram1 = 0

*------------------------------------------------------------------------------
        end
