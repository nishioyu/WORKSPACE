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
      *    ���У������������      (xml2)
      *                        COPY (CPCLAIMED-MANAGEMENTV3REQ.INC)
      ******************************************************************
       01  XML-CLAIMED-MANAGEMENTREQ.
           03  CLAIMQ-CONTEXT         PIC S9(9)   BINARY.
           03  CLAIMQ-OBJECT          PIC X(36).
           03  CLAIMQ-MODE            PIC S9(9)   BINARY.
           03  CLAIMQ-RECORDNAME      PIC X(128).
           03  CLAIMQ-PATIENTREQ.
      *        �ꥯ�������ֹ�
               05  CLAIMQ-REQUEST-NUMBER
                                       PIC X(2).
      *        �����UID
               05  CLAIMQ-KARTE-UID  PIC X(36).
      *        ���륫UID
               05  CLAIMQ-ORCA-UID   PIC X(36).
      *        �»���
               05  CLAIMQ-PERFORM-DATE
                                       PIC X(10).
      *        ���Ծ���
               05  CLAIMQ-PATIENT-INFORMATION.
      *            �����ֹ�
                   07 CLAIMQ-PATIENT-ID  PIC X(20).
      *            ����ǯ��
                   07 CLAIMQ-PERFORM-MONTH
                                           PIC X(7).
      *            �����ʬ
                   07 CLAIMQ-SKYKBN      PIC X(1).
      *            ����ǯ��
                   07 CLAIMQ-SKYYM       PIC X(7).
      *            ����ǯ��
                   07 CLAIMQ-HENREIYM    PIC X(7).
      *            ����ǯ��ʸ�������ʬ��
                   07 CLAIMQ-KOHSKY-SKYYM  PIC X(7).
      *            ����ǯ��ʸ�������ʬ��
                   07 CLAIMQ-KOHSKY-HENREIYM    PIC X(7).
      *            �쥻�ץȾ���
                   07 CLAIMQ-DETAIL-INFORMATION-G.
                       09  CLAIMQ-DETAIL-INFORMATION  OCCURS  50.
      *                    �����
                           11  CLAIMQ-DETAI-TEISYUTUSAKI
                                           PIC X(2).
                           11  CLAIMQ-DETAIL-INOUT
                                           PIC X(1).
      *                    ���̥�����
                           11  CLAIMQ-DETAIL-CODE
                                           PIC X(04).
      *                    ����
                           11  CLAIMQ-DETAIL-TYPE
                                           PIC X(40).
                           11  CLAIMQ-HKN.
      *                        �ݸ��ɣ�
                               13  CLAIMQ-HKNID
                                           PIC X(10).
      *                        �ݸ��ֹ�
                               13  CLAIMQ-HKNNUM
                                           PIC X(03).
      *                    �ݸ�����
                           11  CLAIMQ-MAIN-INSURANCE.
      *                        �ݸ����ֹ�
                               13  CLAIMQ-HKNJANUM
                                           PIC X(08).
      *                        ����
                               13  CLAIMQ-KIGO
                                           PIC X(38).
      *                        �ֹ�
                               13  CLAIMQ-NUM
                                           PIC X(38).
      *                        �����ʬ
                               13  CLAIMQ-HOJOKBN
                                           PIC X(01).
      *                        �ҳ�������̵ͭ
                               13  CLAIMQ-ACCIDENT-TARGET
                                           PIC X(4).
      *                        �������̵ͭ
                               13  CLAIMQ-INCARE-CLASS
                                           PIC X(4).
      *                    �������
                           11  CLAIMQ-PUBLIC-INSURANCE.
                               13  CLAIMQ-PUBLIC-INSURANCE-O  OCCURS  4.
      *                            ����ɣ�
                                   15  CLAIMQ-KOHID
                                           PIC X(10).
      *                            �����ֹ�
                                   15  CLAIMQ-KOHNUM
                                           PIC X(03).
      *                            ��ô���ֹ�
                                   15  CLAIMQ-FTNJANUM
                                           PIC X(08).
      *                            ������ֹ�
                                   15  CLAIMQ-JKYSNUM
                                           PIC X(20).
      *                    ϫ�Ҿ���
                           11  CLAIMQ-ACCIDENT-INSURANCE.
      *                        ϫ�Ҽ����ݸ���ʬ
                               13  CLAIMQ-ACCIDENT
                                           PIC X(01).
      *                        ϫƯ�ݸ��ֹ�
                               13  CLAIMQ-ACCIDENT-NUM
                                           PIC X(14).
      *                         ǯ��ڽ��ֹ�
                               13  CLAIMQ-PENSION-NUM
                                           PIC X(09).
      *                        �ҳ���ʬ
                               13  CLAIMQ-ACCIDENT-CLASS
                                           PIC X(1).
      *                        �򹯴�����Ģ�ֹ�
                               13  CLAIMQ-KANRITCHO-NUMBER
                                          PIC X(13).
      *                        »����ʬ
                               13  CLAIMQ-DAMAGE-CLASS
                                           PIC X(03).
      *                        ����ǯ����
                               13  CLAIMQ-RYOYO-DATE
                                           PIC X(10).
      *                        ���¤�����
                               13  CLAIMQ-DISEASE-LOCATION
                                           PIC X(20).
      *                        ���ܳ�����
                               13  CLAIMQ-START-DATE
                                           PIC X(10).
      *                    ������
                           11  CLAIMQ-LIABILITY-INSURANCE.
      *                        ���¤�����
                               13  CLAIMQ-LI-DISEASE-LOCATION
                                           PIC X(20).
      *                        �ͼ�
                               13  CLAIMQ-FORM-STYLE PIC X(01).
      *                        ���ܳ�����
                               13  CLAIMQ-LI-START-DATE
                                           PIC X(10).
      *                    ����
                           11  CLAIMQ-POLLUTION-INSURANCE.
      *                        ����
                               13  CLAIMQ-POLLUTION-KIGO
                                           PIC X(38).
      *                        �ֹ�
                               13  CLAIMQ-POLLUTION-NUM
                                           PIC X(38).
      *                    �����������
                           11  CLAIMQ-LOCAL-PUBLIC-INSURANCE.
                               13  CLAIMQ-LOCAL-PUBLIC-INS-O
                                                           OCCURS  4.
      *                            ����ɣ�
                                   15  CLAIMQ-LOCAL-KOHID
                                           PIC X(10).
      *                            �����ֹ�
                                   15  CLAIMQ-LOCAL-KOHNUM
                                           PIC X(03).
      *                            ��ô���ֹ�
                                   15  CLAIMQ-LOCAL-FTNJANUM
                                           PIC X(08).
      *                            ������ֹ�
                                   15  CLAIMQ-LOCAL-JKYSNUM
                                           PIC X(20).
      *                    �������
                           11  CLAIMQ-RETURN-INFORMATION.
      *                        ����ǯ��
                               13  CLAIMQ-RE-HENREIYM    PIC X(7).
      *                        �����
                               13  CLAIMQ-RE-TEISYUTUSAKI
                                           PIC X(2).
      *                        �����ʺ������
                               13  CLAIMQ-RE-TEISYUTUSAKI2
                                           PIC X(1).
                               13  CLAIMQ-DETAILL-RETURN-INOUT
                                           PIC X(1).
      *                        ���̥�����
                               13  CLAIMQ-RE-CODE
                                           PIC X(04).
      *                        ����
                               13  CLAIMQ-RE-TYPE
                                           PIC X(40).
      *                        �ݸ�����
                               13  CLAIMQ-RE-MAIN-INSURANCE.
      *                            �ݸ����ֹ�
                                   15  CLAIMQ-RE-HKNJANUM
                                           PIC X(08).
      *                            ����
                                   15  CLAIMQ-RE-KIGO
                                           PIC X(38).
      *                            �ֹ�
                                   15  CLAIMQ-RE-NUM
                                           PIC X(38).
      *                        �ҳ�������̵ͭ
                               13  CLAIMQ-RE-ACCIDENT-TARGET
                                           PIC X(4).
      *                        �������̵ͭ
                               13  CLAIMQ-RE-INCARE-CLASS
                                           PIC X(4).
      *                        �������
                               13  CLAIMQ-RE-PUBLIC-INSURANCE.
                                   15  CLAIMQ-RE-PUBLIC-INS-O
                                                           OCCURS  4.
      *                              ��ô���ֹ�
                                     17  CLAIMQ-RE-FTNJANUM
                                           PIC X(08).
      *                              ������ֹ�
                                     17  CLAIMQ-RE-JKYSNUM
                                           PIC X(20).
      *                        ϫ�Ҿ���
                               13  CLAIMQ-RE-ACCIDENT-INSURANCE.
      *                            ϫ�Ҽ����ݸ���ʬ
                                   15  CLAIMQ-RE-ACCIDENT
                                           PIC X(1).
      *                            ϫƯ�ݸ��ֹ�
                                   15  CLAIMQ-RE-ACCIDENT-NUM
                                           PIC X(14).
      *                             ǯ��ڽ��ֹ�
                                   15 CLAIMQ-RE-PENSION-NUM
                                           PIC X(09).
      *                            �ҳ���ʬ
                                   15  CLAIMQ-RE-ACCIDENT-CLASS
                                           PIC X(1).
      *
