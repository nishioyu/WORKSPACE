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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���У������������    (xml2)
      *                        COPY (CPCLAIM-MANAGEMENTV3RES.INC)
      ******************************************************************
       01  XML-CLAIMED-MANAGEMENTRES.
           03  CLAIMS-CONTEXT         PIC S9(9)   BINARY.
           03  CLAIMS-OBJECT          PIC X(36).
           03  CLAIMS-MODE            PIC S9(9)   BINARY.
           03  CLAIMS-RECORDNAME      PIC X(128).
           03  CLAIMS-RES.
      *        �ꥯ�������ֹ�
               05  CLAIMS-REQUEST-NUMBER
                                       PIC X(2).
      *
               05  CLAIMS-RESPONS-NUMBER
                                       PIC X(2).
      *        �����UID
               05  CLAIMS-KARTE-UID   PIC X(36).
      *        ���륫UID
               05  CLAIMS-ORCA-UID    PIC X(36).
      *        �»���
               05  CLAIMS-INFORMATION-DATE
                                       PIC X(10).
      *        �»ܻ���
               05  CLAIMS-INFORMATION-TIME
                                       PIC X(8).
      *        �ֵѥ�����
               05  CLAIMS-API-RESULT  PIC X(3).
      *        �ֵѥ�å�����
               05  CLAIMS-API-RESULT-MSG
                                       PIC X(100).
      *
      *        ��˥�
               05  CLAIMS-API-WARNING-INFO     OCCURS  5.
                   07  CLAIMS-API-WARNING
                                           PIC X(3).
                   07  CLAIMS-API-WARNING-MSG
                                           PIC X(100).
      *
               05  CLAIMS-RESKEY      PIC X(100).
      *        ���Ծ���
               05  CLAIMS-PATIENT-INFORMATION.
      *            �����ֹ�
                   07  CLAIMS-PATIENT-ID   PIC X(20).
      *            ���Ի�̾
                   07  CLAIMS-NAME         PIC X(100).
      *            ����ǯ��
                   07  CLAIMS-PERFORM-MONTH
                                           PIC X(7).
      *            �����ʬ
                   07  CLAIMS-SKYKBN      PIC X(1).
      *            ����ǯ��
                   07  CLAIMS-SKYYM       PIC X(7).
      *            ����ǯ��
                   07  CLAIMS-HENREIYM    PIC X(7).
      *            ����ǯ��ʸ�������ʬ��
                   07  CLAIMS-KOHSKY-SKYYM PIC X(7).
      *            ����ǯ��ʸ�������ʬ��
                   07  CLAIMS-KOHSKY-HENREIYM    PIC X(7).
      *            �쥻�ץȾ���
                   07  CLAIMS-RECEIPT-INFORMATION.
                     08 CLAIMS-RECEIPT-DETAIL-O OCCURS  50.
                       09  CLAIMS-RECEIPT-ERRCD
                                               PIC X(3).
                       09  CLAIMS-RECEIPT-ERR-MSG
                                           PIC X(100).
      *                �����
                       09  CLAIMS-DETAIL-TEISYUTUSAKI
                                           PIC X(2).
                       09  CLAIMS-DETAIL-INOUT
                                           PIC X(1).
      *                ���̥�����
                       09  CLAIMS-DETAIL-CODE
                                           PIC X(04).
      *                ����
                       09  CLAIMS-DETAIL-TYPE
                                           PIC X(40).
                       09  CLAIMS-DETAIL-SKYKBN
                                           PIC X(1).
      *                ����ǯ��
                       09  CLAIMS-RECEIPT-SKYYM       PIC X(7).
      *                ����ǯ��
                       09  CLAIMS-RECEIPT-HENREIYM    PIC X(7).
      *                �ݸ��ɣ�
                       09  CLAIMS-HKNID    PIC X(10).
      *                �ݸ��ֹ�
                       09  CLAIMS-HKNNUM   PIC X(03).
      *                �ݸ�����
                       09  CLAIMS-MAIN-INSURANCE.
                           11  CLAIMS-MAIN-INSURANCE-1.
      *                        �ݸ����ֹ�
                               13  CLAIMS-HKNJANUM
                                           PIC X(08).
      *                        ����
                               13  CLAIMS-KIGO
                                           PIC X(38).
      *                        �ֹ�
                               13  CLAIMS-NUM
                                           PIC X(38).
      *                        �����ʬ
                               13  CLAIMS-HOJOKBN
                                           PIC X(01).
      *                    �ҳ�������̵ͭ
                           11  CLAIMS-ACCIDENT-TARGET
                                           PIC X(4).
      *                    �������̵ͭ
                           11  CLAIMS-INCARE-CLASS
                                           PIC X(4).
      *                �������
                       09  CLAIMS-PUBLIC-INSURANCE.
                           11  CLAIMS-PUBLIC-INSURANCE-O  OCCURS  4.
      *                        ����ɣ�
                               13  CLAIMS-KOHID
                                           PIC X(10).
      *                        �����ֹ�
                               13  CLAIMS-KOHNUM
                                           PIC X(03).
      *                        ��ô���ֹ�
                               13  CLAIMS-FTNJANUM
                                           PIC X(08).
      *                        ������ֹ�
                               13  CLAIMS-JKYSNUM
                                           PIC X(20).
      *                ϫ�Ҿ���
                       09  CLAIMS-ACCIDENT-INSURANCE.
      *                    ϫ�Ҽ����ݸ���ʬ
                           11  CLAIMS-ACCIDENT
                                           PIC X(01).
      *                    ϫƯ�ݸ��ֹ�
                           11  CLAIMS-ACCIDENT-NUM
                                           PIC X(14).
      *                     ǯ��ڽ��ֹ�
                           11  CLAIMS-PENSION-NUM
                                           PIC X(09).
      *                    �ҳ���ʬ
                           11  CLAIMS-ACCIDENT-CLASS
                                           PIC X(1).
      *                    �򹯴�����Ģ�ֹ�
                           11  CLAIMS-KANRITCHO-NUMBER
                                           PIC X(13).
      *                    »����ʬ
                           11  CLAIMS-DAMAGE-CLASS
                                           PIC X(03).
      *                    ����ǯ����
                           11  CLAIMS-RYOYO-DATE
                                           PIC X(10).
      *                    ���¤�����
                           11  CLAIMS-DISEASE-LOCATION
                                           PIC X(20).
      *                    ���ܳ�����
                           11  CLAIMS-START-DATE
                                           PIC X(10).
      *                ������
                       09  CLAIMS-LIABILITY-INSURANCE.
      *                    ���¤�����
                           11  CLAIMS-LI-DISEASE-LOCATION
                                           PIC X(20).
      *                    �ͼ�
                           11  CLAIMS-FORM-STYLE PIC X(01).
      *                    ���ܳ�����
                           11  CLAIMS-LI-START-DATE
                                           PIC X(10).
      *                ����
                       09  CLAIMS-POLLUTION-INSURANCE.
      *                    ����
                           11  CLAIMS-POLLUTION-KIGO
                                           PIC X(38).
      *                    �ֹ�
                           11  CLAIMS-POLLUTION-NUM
                                           PIC X(38).
      *                �����������
                       09  CLAIMS-LOCAL-PUBLIC-INSURANCE.
      *                    ����ǯ��
                           11  CLAIMS-LOCAL-PUBLIC-SKYYM     PIC X(7).
      *                    ����ǯ��
                           11  CLAIMS-LOCAL-PUBLIC-HENREIYM  PIC X(7).
                           11  CLAIMS-LOCAL-PUBLIC-O OCCURS  4.
      *                        ����ɣ�
                               15  CLAIMS-LOCAL-KOHID
                                           PIC X(10).
      *                        �����ֹ�
                               15  CLAIMS-LOCAL-KOHNUM
                                           PIC X(03).
      *                        ��ô���ֹ�
                               15  CLAIMS-LOCAL-FTNJANUM
                                           PIC X(08).
      *                        ������ֹ�
                               15  CLAIMS-LOCAL-JKYSNUM
                                           PIC X(20).
      *
      *                ��������ɳ�դ�����ѡ�
                       09  CLAIMS-RETURN-INFORMATION.
      *                    ����ǯ��
                           11  CLAIMS-RE-HENREIYM
                                           PIC X(7).
      *                        �����
                           11  CLAIMS-RE-TEISYUTUSAKI
                                           PIC X(2).
      *                        �����ʺ������
                           11  CLAIMS-RE-TEISYUTUSAKI2
                                           PIC X(1).
                           11  CLAIMS-RE-INOUT
                                           PIC X(1).
      *                        ���̥�����
                           11  CLAIMS-RE-CODE
                                           PIC X(04).
      *                        ����
                           11  CLAIMS-RE-TYPE
                                           PIC X(40).
      *                        �ݸ�����
                           11  CLAIMS-RE-MAIN-INSURANCE.
      *                            �ݸ����ֹ�
                               13  CLAIMS-RE-HKNJANUM
                                           PIC X(08).
      *                            ����
                               13  CLAIMS-RE-KIGO
                                           PIC X(38).
      *                            �ֹ�
                               13  CLAIMS-RE-NUM
                                           PIC X(38).
      *                        �ҳ�������̵ͭ
                           11  CLAIMS-RE-ACCIDENT-TARGET
                                           PIC X(4).
      *                        �������̵ͭ
                           11  CLAIMS-RE-INCARE-CLASS
                                           PIC X(4).
      *                        �������
                           11  CLAIMS-RE-PUBLIC-INSURANCE.
                               13  CLAIMS-RE-PUBLIC-INSURANCE-O
                                                             OCCURS  4.
      *                            ��ô���ֹ�
                                   15  CLAIMS-RE-PROVIDER-NUMBER
                                           PIC X(08).
      *                            ������ֹ�
                                   15  CLAIMS-RE-RECIPIENT-NUMBER
                                           PIC X(20).
      *                    ϫ�Ҿ���
                           11  CLAIMS-RE-ACCIDENT-INSURANCE.
      *                        ϫ�Ҽ����ݸ���ʬ
                               13  CLAIMS-RE-ACCIDENT
                                           PIC X(1).
      *                        ϫƯ�ݸ��ֹ�
                               13  CLAIMS-RE-ACCIDENT-NUM
                                           PIC X(14).
      *                        ǯ��ڽ��ֹ�
                               13 CLAIMS-RE-PENSION-NUM
                                           PIC X(09).
      *                        �ҳ���ʬ
                               13  CLAIMS-RE-ACCIDENT-CLASS
                                           PIC X(1).
      *                        �����ͳ
                               13  CLAIMS-RE-ERRMSG
                                           PIC X(100).
      *            �������
                   07  CLAIMS-RE-DETAIL-INFORMATION.
                       09  CLAIMS-RE-DET-INFORMATION-G OCCURS  30.
      *???                 ���������
      *???                 11  CLAIMS-RE-DET-HENREI-FLG
      *???                                  PIC X(05).
      *                    ����ǯ��
                           11  CLAIMS-RE-DET-HENREIYM
                                           PIC X(7).
      *                        �����
                           11  CLAIMS-RE-DET-TEISYUTUSAKI
                                           PIC X(2).
      *                        �����ʺ������
                           11  CLAIMS-RE-DET-TEISYUTUSAKI2
                                           PIC X(1).
                           11  CLAIMS-RE-DET-INOUT
                                           PIC X(1).
      *                        ���̥�����
                           11  CLAIMS-RE-DET-CODE
                                           PIC X(04).
      *                        ����
                           11  CLAIMS-RE-DET-TYPE
                                           PIC X(40).
      *                        �ݸ�����
                           11  CLAIMS-RE-DET-MAIN-INSURANCE.
      *                        �ݸ����ֹ�
                               13  CLAIMS-RE-DET-INSURANCE-NUM
                                           PIC X(08).
      *                        ����
                               13  CLAIMS-RE-DET-KIGO
                                           PIC X(38).
      *                         ��
                               13  CLAIMS-RE-DET-NUM
                                           PIC X(38).
      *                    �ҳ�������̵ͭ
                           11  CLAIMS-RE-DET-ACCIDENT-TARGET
                                           PIC X(4).
      *                    �������̵ͭ
                           11  CLAIMS-RE-DET-INCARE-CLASS
                                           PIC X(4).
      *                    �������
                           11  CLAIMS-RE-DET-PUBLIC-INS.
                               13  CLAIMS-RE-DET-PUBLIC-INS-O
                                                             OCCURS  4.
      *                            ��ô���ֹ�
                                   15  CLAIMS-RE-DET-PROVIDER-NUM
                                           PIC X(08).
      *                            ������ֹ�
                                   15  CLAIMS-RE-DET-RECIPIENT-NUM
                                           PIC X(20).
      *                    ϫ�Ҿ���
                           11  CLAIMS-RE-DET-ACCIDENT-INS.
      *                        ϫ�Ҽ����ݸ���ʬ
                               13  CLAIMS-RE-DET-ACCIDENT
                                           PIC X(1).
      *                        ϫƯ�ݸ��ֹ�
                               13  CLAIMS-RE-DET-ACCIDENT-NUM
                                           PIC X(14).
      *                        ǯ��ڽ��ֹ�
                               13  CLAIMS-RE-DET-PENSION-NUM
                                           PIC X(09).
      *                        �ҳ���ʬ
                               13  CLAIMS-RE-DET-ACCIDENT-CLASS
                                           PIC X(1).
      *                        �����ͳ
                               13  CLAIMS-RE-DET-ERRMSG
                                           PIC X(100).
      *            ���顼�쥻�ץȾ���
                   07  CLAIMS-ERR-INFO-G.
                       09  CLAIMS-ERR-INFORMATION  OCCURS  50.
      *                  ���顼������
                         10  CLAIMS-ERR-ERRCD
                                           PIC X(3).
      *                  �����
                         10  CLAIMS-ERR-ERRMSG
                                           PIC X(100).
                         10  CLAIMS-ERR-DETAIL-INFORMATION.
      *                    �����
                           11  CLAIMS-ERR-TEISYUTUSAKI
                                           PIC X(2).
                           11  CLAIMS-ERR-INOUT
                                           PIC X(1).
      *                    ���̥�����
                           11  CLAIMS-ERR-CODE
                                           PIC X(04).
      *                    ����
                           11  CLAIMS-ERR-TYPE
                                           PIC X(40).
      *                    �ݸ�����
                           11  CLAIMS-ERR-MAIN-INSURANCE.
      *                        �ݸ��ɣ�
                               13  CLAIMS-ERR-HKNID
                                           PIC X(10).
      *                        �ݸ��ֹ�
                               13  CLAIMS-ERR-INS-CLASS
                                           PIC X(03).
                               13 CLAIMS-ERR-MAIN-INSURANCE-1.
      *                            �ݸ����ֹ�
                                   15 CLAIMS-ERR-HKNJANUM
                                           PIC X(08).
      *                            ����
                                   15 CLAIMS-ERR-KIGO
                                           PIC X(38).
      *                            �ֹ�
                                   15 CLAIMS-ERR-NUM
                                           PIC X(38).
      *                            �����ʬ
                                   15 CLAIMS-ERR-HOJOKBN
                                           PIC X(01).
      *                        �ҳ�������̵ͭ
                               13 CLAIMS-ERR-ACCIDENT-TARGET
                                           PIC X(4).
      *                        �������̵ͭ
                               13 CLAIMS-ERR-INCARE-CLASS
                                           PIC X(4).
      *                    �������
                           11 CLAIMS-ERR-PUBLIC-INSURANCE.
                               13 CLAIMS-ERR-PUBLIC-INSURANCE-O
                                                           OCCURS  4.
      *                            ����ɣ�
                                   15 CLAIMS-ERR-KOHID
                                           PIC X(10).
      *                            �����ֹ�
                                   15 CLAIMS-ERR-KOHNUM
                                           PIC X(03).
      *                            ��ô���ֹ�
                                   15 CLAIMS-ERR-FTNJANUM
                                           PIC X(08).
      *                            ������ֹ�
                                   15 CLAIMS-ERR-JKYSNUM
                                           PIC X(20).
      *                    ϫ�Ҿ���
                           11 CLAIMS-ERR-ACCIDENT-INSURANCE.
      *                        ϫ�Ҽ����ݸ���ʬ
                               13 CLAIMS-ERR-ACCIDENT
                                           PIC X(01).
      *                        ϫƯ�ݸ��ֹ�
                               13 CLAIMS-ERR-ACCIDENT-NUM
                                           PIC X(14).
      *                         ǯ��ڽ��ֹ�
                               13 CLAIMS-ERR-PENSION-NUM
                                           PIC X(09).
      *                        �ҳ���ʬ
                               13 CLAIMS-ERR-ACCIDENT-CLASS
                                           PIC X(1).
      *                        �򹯴�����Ģ�ֹ�
                               13 CLAIMS-ERR-KANRITCHO-NUMBER
                                          PIC X(13).
      *                        »����ʬ
                               13 CLAIMS-ERR-DAMAGE-CLASS
                                           PIC X(03).
      *                        ����ǯ����
                               13 CLAIMS-ERR-RYOYO-DATE
                                           PIC X(10).
      *                        ���¤�����
                               13 CLAIMS-ERR-DISEASE-LOCATION
                                           PIC X(20).
      *                        ���ܳ�����
                               13 CLAIMS-ERR-START-DATE
                                           PIC X(10).
      *                    ������
                           11 CLAIMS-ERR-LIABILITY-INS.
      *                        ���¤�����
                               13  CLAIMS-ERR-LI-DISEASE-LOCATION
                                           PIC X(20).
      *                        �ͼ�
                               13  CLAIMS-ERR-FORM-STYLE PIC X(01).
      *                        ���ܳ�����
                               13  CLAIMS-ERR-LI-START-DATE
                                           PIC X(10).
      *                    ����
                           11  CLAIMS-ERR-POLLUTION-INS.
      *                        ����
                               13  CLAIMS-ERR-POLLUTION-KIGO
                                           PIC X(38).
      *                        �ֹ�
                               13  CLAIMS-ERR-POLLUTION-NUM
                                           PIC X(38).
      *                    �����������
                           11  CLAIMS-ERR-PUBLIC-INSURANCE.
                               13  CLAIMS-ERR-PUBLIC-INSURANCE-O
                                                           OCCURS  4.
      *                            ����ɣ�
                                   15 CLAIMS-ERR-KOHID
                                           PIC X(10).
      *                            �����ֹ�
                                   15 CLAIMS-ERR-KOHNUM
                                           PIC X(03).
      *                            ��ô���ֹ�
                                   15 CLAIMS-ERR-FTNJANUM
                                           PIC X(08).
      *                            ������ֹ�
                                   15 CLAIMS-ERR-JKYSNUM
                                           PIC X(20).
      *                    �������
                           11  CLAIMS-ERR-RET-INFORMATION.
      *                        ����ǯ��
                               13  CLAIMS-ERR-RET-HENREIYM
                                           PIC X(7).
      *                        �����
                               13  CLAIMS-ERR-RET-TEISYUTUSAKI
                                           PIC X(2).
      *                        �����ʺ������
                               13 CLAIMS-ERR-RET-TEISYUTUSAKI2
                                           PIC X(1).
                               13 CLAIMS-ERR-RET-INOUT
                                           PIC X(1).
      *                        ���̥�����
                               13  CLAIMS-ERR-RET-CODE
                                           PIC X(04).
      *                        ����
                               13  CLAIMS-ERR-RET-TYPE
                                           PIC X(40).
      *                        �ݸ�����
                               13  CLAIMS-ERR-RET-MAIN-INSURANCE.
      *                            �ݸ����ֹ�
                                   15  CLAIMS-ERR-RET-HKNJANUM
                                           PIC X(08).
      *                            ����
                                   15  CLAIMS-ERR-RET-KIGO
                                           PIC X(38).
      *                            �ֹ�
                                   15  CLAIMS-ERR-RET-NUM
                                           PIC X(38).
      *                        �ҳ�������̵ͭ
                               13  CLAIMS-ERR-RET-ACCIDENT-TARGET
                                           PIC X(4).
      *                        �������̵ͭ
                               13  CLAIMS-ERR-RET-INCARE-CLASS
                                           PIC X(4).
      *                        �������
                               13  CLAIMS-ERR-RET-PUBLIC-INS.
                                   15  CLAIMS-ERR-RET-PUBLIC-INS-O
                                                           OCCURS  4.
      *                                ��ô���ֹ�
                                       17  CLAIMS-ERR-RET-PROVIDER-NUM
                                           PIC X(08).
      *                                ������ֹ�
                                       17  CLAIMS-ERR-RET-RECIPIENT-NUM
                                           PIC X(20).
      *                        ϫ�Ҿ���
                               13  CLAIMS-ERR-RET-ACCIDENT-INS.
      *                            ϫ�Ҽ����ݸ���ʬ
                                   15  CLAIMS-ERR-RET-ACCIDENT
                                           PIC X(1).
      *                            ϫƯ�ݸ��ֹ�
                                   15  CLAIMS-ERR-RET-ACCIDENT-NUM
                                           PIC X(14).
      *                             ǯ��ڽ��ֹ�
                                   15  CLAIMS-ERR-RET-PENSION-NUM
                                           PIC X(09).
      *                            �ҳ���ʬ
                                   15  CLAIMS-ERR-RET-ACCIDENT-CLASS
                                           PIC X(1).
      *                        �����ͳ
                               13 CLAIMS-ERR-RET-ERRMSG
                                           PIC X(100).
