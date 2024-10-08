      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSYAKINGEN.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ��л���Ķ�Ḻ�������������ɼ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  14/02/27    NACL�ݾ�Ʀ��  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *ʿ������ǯ��������б�
      *  00.00.00    NACL-��Ʀ��  18/03/01  �����������������ο���
      *                                     �ϰ�����������ο���
      *                                     ����������������������ʿ��
      *                                     ����ǯ����ޤ�
      *  05.02.02    ORCAMO       24/05/14  ���£�ǯ�ٲ����б�
      *                                     �㳲�Ի������������������ɲ�
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ����
           COPY    "CPSK5001.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ��л���Ķ�Ḻ�������������ɼ����ѥ�᥿
           COPY    "CPORCSYAKINGEN.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSYAKINGEN-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  LNK-YAKINGEN-SANTEIYM       >=   "201404"
      *        �����ƥ������������л���Ķ�Ḻ���ζ�ʬ������
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                      SYS-5001-REC
               MOVE    "5001"              TO  SYS-5001-KANRICD
               MOVE    LNK-YAKINGEN-BTU-NUM
                                           TO  SYS-5001-KBNCD
               MOVE    LNK-YAKINGEN-SANTEIYM
                                           TO  SYS-5001-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
               MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
               MOVE    SYS-5001-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    ZERO                TO  FLG-SYSKANRI
                       MOVE    MCPDATA-REC         TO  SYS-5001-REC
                   ELSE
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               ELSE
                   MOVE    SPACE               TO  SYS-5001-REC
                   INITIALIZE                  SYS-5001-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF    SYS-5001-YKNCHOKAKBN  =   "1"
                   IF    LNK-YAKINGEN-SRYCD    NOT  =  SPACE
                       IF  LNK-YAKINGEN-SANTEIYM       >=   "201804"
                           PERFORM    201-CDCHG-SEC
                       ELSE
                           PERFORM    200-CDCHG-SEC
                       END-IF
                   END-IF
      *
               END-IF
           END-IF
      *
           IF    LNK-YAKINGEN-CHGSRYCD   =     SPACE
               MOVE    1                 TO     LNK-YAKINGEN-RC
           END-IF
      *
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    �������֤����������Ķ�Ḻ����ʿ������ǯ�����ʿ������ǯ����ޤ�
      *****************************************************************
       200-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-YAKINGEN-SRYCD
      *        ��������У�����������
               WHEN    "190117710"
                  MOVE "190131310" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190077410"
                  MOVE "190131410" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190077510"
                  MOVE "190165810" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190077610"
                  MOVE "190165910" TO  LNK-YAKINGEN-CHGSRYCD
      *        ������������������������������������
               WHEN    "190132410"
                  MOVE "190166110" TO  LNK-YAKINGEN-CHGSRYCD
      *        �����������������������������������ˡ��������ܡ�
               WHEN    "190132510"
                  MOVE "190166210" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������¡�
               WHEN    "190132610"
                  MOVE "190166310" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������¡ˡ��������ܡ�
               WHEN    "190132710"
                  MOVE "190166410" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������á�
               WHEN    "190132810"
                  MOVE "190166510" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������áˡ��������ܡ�
               WHEN    "190132910"
                  MOVE "190166610" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ġ�
               WHEN    "190133010"
                  MOVE "190166710" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ġˡ��������ܡ�
               WHEN    "190133110"
                  MOVE "190166810" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������š�
               WHEN    "190133210"
                  MOVE "190166910" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������šˡ��������ܡ�
               WHEN    "190133310"
                  MOVE "190167010" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ơ�
               WHEN    "190133410"
                  MOVE "190167110" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ơˡ��������ܡ�
               WHEN    "190133510"
                  MOVE "190167210" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ǡ�
               WHEN    "190133610"
                  MOVE "190167310" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ǡˡ��������ܡ�
               WHEN    "190133710"
                  MOVE "190167410" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ȡ�
               WHEN    "190133810"
                  MOVE "190167510" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ȡˡ��������ܡ�
               WHEN    "190133910"
                  MOVE "190167610" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ɡ�
               WHEN    "190134010"
                  MOVE "190167710" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������������������������ɡˡ��������ܡ�
               WHEN    "190134110"
                  MOVE "190167810" TO  LNK-YAKINGEN-CHGSRYCD
      *        �������У�����������
               WHEN    "190117910"
                  MOVE "190134310" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081710"
                  MOVE "190134410" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081810"
                  MOVE "190168110" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081910"
                  MOVE "190168210" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190082010"
                  MOVE "190168310" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190082110"
                  MOVE "190168410" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083610"
                  MOVE "190134610" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190134510"
                  MOVE "190168510" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083810"
                  MOVE "190168610" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083910"
                  MOVE "190168710" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190084010"
                  MOVE "190168810" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի��������У�����������
               WHEN    "190126310"
                  MOVE "190169210" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У�����������
               WHEN    "190093710"
                  MOVE "190169310" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У�����������
               WHEN    "190093810"
                  MOVE "190169410" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У�����������
               WHEN    "190093910"
                  MOVE "190169510" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������ʾ㳲�Ի�����������������
               WHEN    "190161210"
                  MOVE "190161310" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի��������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189410"
                  MOVE "190198910" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի��������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189510"
                  MOVE "190199010" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189610"
                  MOVE "190199110" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189710"
                  MOVE "190199210" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189810"
                  MOVE "190199310" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189910"
                  MOVE "190199410" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190190010"
                  MOVE "190199510" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190190110"
                  MOVE "190199610" TO  LNK-YAKINGEN-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       200-CDCHG-EXT.
           .
      *
      *****************************************************************
      *    �������֤����������Ķ�Ḻ���� ʿ������ǯ�����
      *****************************************************************
       201-CDCHG-SEC                    SECTION.
      *
           EVALUATE    LNK-YAKINGEN-SRYCD
      *        ������������������
               WHEN    "190117710"
                  MOVE "190131310" TO  LNK-YAKINGEN-CHGSRYCD
      *        ������������������
               WHEN    "190199710"
                  MOVE "190200310" TO  LNK-YAKINGEN-CHGSRYCD
      *        ������������������
               WHEN    "190199810"
                  MOVE "190200410" TO  LNK-YAKINGEN-CHGSRYCD
      *        ������������������
               WHEN    "190199910"
                  MOVE "190200510" TO  LNK-YAKINGEN-CHGSRYCD
      *        ������������������
               WHEN    "190200010"
                  MOVE "190200610" TO  LNK-YAKINGEN-CHGSRYCD
      *        ������������������
               WHEN    "190200110"
                  MOVE "190200710" TO  LNK-YAKINGEN-CHGSRYCD
      *        ������������������
               WHEN    "190077410"
                  MOVE "190131410" TO  LNK-YAKINGEN-CHGSRYCD
      *        �ϰ������������
               WHEN    "190200210"
                  MOVE "190200810" TO  LNK-YAKINGEN-CHGSRYCD
      *        �ϰ������������
               WHEN    "190077510"
                  MOVE "190165810" TO  LNK-YAKINGEN-CHGSRYCD
      *        �ϰ������������
               WHEN    "190077610"
                  MOVE "190165910" TO  LNK-YAKINGEN-CHGSRYCD
      *        �������У�����������
               WHEN    "190117910"
                  MOVE "190134310" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081710"
                  MOVE "190134410" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081810"
                  MOVE "190168110" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190081910"
                  MOVE "190168210" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190082010"
                  MOVE "190168310" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������У�����������
               WHEN    "190082110"
                  MOVE "190168410" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083610"
                  MOVE "190134610" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190134510"
                  MOVE "190168510" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083810"
                  MOVE "190168610" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190083910"
                  MOVE "190168710" TO  LNK-YAKINGEN-CHGSRYCD
      *        ����������У�����������
               WHEN    "190084010"
                  MOVE "190168810" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի��������У�����������
               WHEN    "190126310"
                  MOVE "190169210" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У�����������
               WHEN    "190093710"
                  MOVE "190169310" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У�����������
               WHEN    "190093810"
                  MOVE "190169410" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У�����������
               WHEN    "190093910"
                  MOVE "190169510" TO  LNK-YAKINGEN-CHGSRYCD
      *        ���������������ʾ㳲�Ի�����������������
               WHEN    "190161210"
                  MOVE "190161310" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի��������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189410"
                  MOVE "190198910" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի��������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189510"
                  MOVE "190199010" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189610"
                  MOVE "190199110" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189710"
                  MOVE "190199210" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189810"
                  MOVE "190199310" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190189910"
                  MOVE "190199410" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190190010"
                  MOVE "190199510" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ����
               WHEN    "190190110"
                  MOVE "190199610" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի��������У������������ʰ��Ŷ�ʬ���ˡʽ��ٽ�����
               WHEN    "190240610"
                  MOVE "190241410" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի��������У������������ʰ��Ŷ�ʬ���ˡʽ��ٽ�����
               WHEN    "190240710"
                  MOVE "190241510" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ���ˡʽ��ٽ�����
               WHEN    "190240810"
                  MOVE "190241610" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ���ˡʽ��ٽ�����
               WHEN    "190240910"
                  MOVE "190241710" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ���ˡʽ��ٽ�����
               WHEN    "190241010"
                  MOVE "190241810" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ���ˡʽ��ٽ�����
               WHEN    "190241110"
                  MOVE "190241910" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ���ˡʽ��ٽ�����
               WHEN    "190241210"
                  MOVE "190242010" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У������������ʰ��Ŷ�ʬ���ˡʽ��ٽ�����
               WHEN    "190241310"
                  MOVE "190242110" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի��������У�������������������¡�¤δ��ԡ�
               WHEN    "190830610"
                  MOVE "190831010" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У�������������������¡�¤δ��ԡ�
               WHEN    "190830710"
                  MOVE "190831110" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У�������������������¡�¤δ��ԡ�
               WHEN    "190830810"
                  MOVE "190831210" TO  LNK-YAKINGEN-CHGSRYCD
      *        �㳲�Ի����������У�������������������¡�¤δ��ԡ�
               WHEN    "190830910"
                  MOVE "190831310" TO  LNK-YAKINGEN-CHGSRYCD
               WHEN    OTHER
                       CONTINUE
           END-EVALUATE
      *
           .
      *
       201-CDCHG-EXT.
           .
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
