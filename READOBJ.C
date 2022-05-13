/****************************************************************
* name         readobj -- read and format obj files             *
*                         into a printable output               *
*                                                               *
* Original by Kip Davidson (73247,620)                          *
* Upgraded to handle OS/2 object & libs by Dave Cortesi (72155,450) *
*                                                               *
* usage        readobj filename                                 *
*                                                               *
*              where "filename" is the name of an object code   *
*              file, e.g. ROB.OBJ, or else the name of an       *
*              object library, e.g. DOSCALLS.LIB.               *
*                                                               *
****************************************************************/

/****************
* Include files *
*****************/
#define LINT_ARGS
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

/********************************
* Microsoft Object Record Types *
********************************/
#define BLKDEF  0x7A
#define BLKEND  0x7C
#define THEADR  0x80
#define COMENT  0x88
#define MODEND  0x8A
#define EXTDEF  0x8C
#define TYPDEF  0x8E
#define PUBDEF  0x90
#define LINNUM  0x94
#define LNAMES  0x96
#define SEGDEF  0x98
#define GRPDEF  0x9A
#define FIXUPP  0x9C
#define LEDATA  0xA0
#define LIDATA  0xA2
#define COMDEF  0xB0    /* communal-names definition */

/********************************
* Intel Object Record Types     *
********************************/
#define RHEADR  0x6E
#define REGINT  0x70
#define REDATA  0x72
#define RIDATA  0x74
#define OVLDEF  0x76
#define ENDREC  0x78
#define DEBSYM  0x7E
#define LHEADR  0x82
#define PEDATA  0x84
#define PIDATA  0x86
#define LOCSYM  0x92
#define LIBHED  0xA4
#define LIBNAM  0xA6
#define LIBLOC  0xA8
#define LIBDIC  0xAA

/************************
* Miscellaneous defines *
************************/
#define NUM_ARGS  2
#define MAX_LNAME_ENTRIES  100
#define MAX_NAME_LEN        64
#define MAX_SNAME_ENTRIES  100
#define SNAME_SEG   0
#define SNAME_CLASS 1
#define SNAME_OVL   2
#define GRPDEF_SI   0xff
#define CV_ARRAY  0x6281   /* Codeview array indicator? */
#define BLANK   ' '
#define ZERO    0x00
#define NOSWAP  00
#define SWAP    01
#define BIT7    0x80
#define BIT6    0x40
#define BIT5    0x20
#define BIT4    0x10
#define BIT3    0x08
#define BIT2    0x04
#define BIT1    0x02
#define BIT0    0x01

/*********************
* Error code defines *
*********************/
#define USAGE_ERR  1
#define OPEN_ERR   2

/*********
* Macros *
**********/
#define MALLOC(x)      ((x *) malloc(sizeof(x)))
#define CALLOC(n,x)    ((x *) calloc(n, sizeof(x)))

/***********
* Typedefs *
************/
typedef unsigned char byte;
typedef unsigned char * p_byte;     /* pointer to a byte */
typedef unsigned short word;
typedef unsigned short * p_word;    /* pointer to a word */

/****************
* File pointers *
****************/
FILE *fp1;              /* object code file pointer */

/*******************
* Global variables *
*******************/
word stoppit;          /* flag for end of file */
word i=0,j=0,k=0;      /* misc index counters */
byte c=0,c1=0,c2=0;    /* misc unsigned char variables */
word reclen=0;         /* length of record */
byte num_bytes=0;      /* length of string passed */
word chksum_count=0;   /* total bytes read for a record */
byte chksum=0;         /* the checksum from the obj file */
byte Lnames_index=1;   /* number of LNAMES entries */
byte Lnames_total=0;   /* number of LNAMES entries */
                       /* storage for LNAMES data */
byte Lnames [MAX_LNAME_ENTRIES] [MAX_NAME_LEN+1] =
{'*','N','U','L','L','*','\0'};
byte Snames_index=1;   /* number of SNAMES entries */
byte Snames_total=0;   /* number of SNAMES entries */
                       /* storage for SNAMES data */
word Snames [MAX_SNAME_ENTRIES] [SNAME_OVL+1] =
{ {0,0,0} };
byte *align_msg[] =    /* SEGDEF align messages */
{
  "0 Absolute segment",
  "1 Relocatable and byte-aligned segment",
  "2 Relocatable and word-aligned segment",
  "3 Relocatable and paragraph-aligned segment",
  "4 Relocatable and page-aligned segment",
};
byte *combine_msg[] =  /* SEGDEF combine messages */
{
  "0 Private segment",
  "1 ?",
  "2 Public segment",
  "3 ?",
  "4 ?",
  "5 Stack segment",
  "6 Common segment",
};

byte chr_buff[256];    /* string buffer */

/**************************************************************
* This program uses the Object Record Types as described in   *
* the following documents :                                   *
*                                                             *
* Intel :     8086 Relocatable Object Module Formats          *
*             Version 4.0, Order # 121748-001                 *
* Microsoft : MS-DOS Programmers Reference                    *
*             Document # 8411-310-02                          *
*             Part # 036-014-012                              *
***************************************************************/
word main(argc,argv)
word argc;
byte *argv[];
{
void do_blkdef(void);
void do_blkend(void);
void do_comdef(void);
void do_coment(void);
void do_extdef(void);
void do_fixupp(void);
void do_grpdef(void);
void do_ledata(void);
void do_lidata(void);
void do_linnum(void);
void do_lnames(void);
void do_modend(void);
void do_pubdef(void);
void do_locsym(void);
void do_segdef(void);
void do_theadr(void);
void do_typdef(void);
void do_unimplt(void);
void exit(int);
byte get_obj_byte(void);

if (argc != NUM_ARGS)
   {
    fputs("Usage: readobj filename \n", stderr);
    exit(USAGE_ERR);
   }

if ((fp1 = fopen(argv[1], "rb")) == NULL)
   {
    fprintf(stderr,"Can't open %s\n",argv[1]);
    exit(OPEN_ERR);
   }

printf("READOBJ by Kip Davidson (73247,620)\n");
printf("\tOS/2 version Dave Cortesi (72155,450)\n\n");

c = get_obj_byte();
while (stoppit == 0)
   {
    switch (c)
     {
      case BLKDEF :
            do_blkdef();
            break;
      case BLKEND :
            do_blkend();
            break;
      case THEADR :
            do_theadr();
            break;
      case COMENT :
            do_coment();
            break;
      case MODEND :
            do_modend();
            break;
      case EXTDEF :
            do_extdef();
            break;
      case TYPDEF :
            do_typdef();
            break;
      case PUBDEF :
            do_pubdef();
            break;
      case LOCSYM :
            do_locsym();
            break;
      case LINNUM :
            do_linnum();
            break;
      case LNAMES :
            do_lnames();
            break;
      case SEGDEF :
            do_segdef();
            break;
      case GRPDEF:
            do_grpdef();
            break;
      case FIXUPP :
            do_fixupp();
            break;
      case LEDATA :
            do_ledata();
            break;
      case LIDATA :
            do_lidata();
            break;
      case COMDEF :
            do_comdef();
            break;
      case 0x00 : /* null bytes used in LIB files for padding */
            break; /* skip 'em, a THEADR will show up soon */
      default :
            do_unimplt();
            break;
     }
     c = get_obj_byte();
   }
return(0);
}

/************************************************
* get a byte from the object code file pointer  *
************************************************/
byte get_obj_byte()
{
word w;
 c = (byte)(w =  getc(fp1));
 stoppit = (w == EOF);
 chksum_count = chksum_count + c;
 return(c);
}

/************************************************
* get a word from the object code file pointer  *
************************************************/
word get_obj_word(swap)
byte swap;
{                                /* input -  Hi Lo */
 if (swap)
  {
   c1 = get_obj_byte();
   c2 = get_obj_byte();
   return( (c2 << 8) + c1);       /* swap - Lo Hi */
  }
 else
  {
   c1 = get_obj_byte();
   c2 = get_obj_byte();
   return( (c1 << 8) + c2);       /* noswap - Hi Lo */
  }
}

/****************************************************
* return the length of the contents field in bytes  *
****************************************************/
word get_reclen()
{
word get_obj_word(byte);

 reclen = get_obj_word(SWAP);
 printf(" (reclen %04x)\n",reclen);
 return(reclen);
}

/*********************
* check the checksum; print only if wrong *
*********************/
void do_checksum()
{
byte get_obj_byte(void);

 chksum = get_obj_byte();
 /* printf(" checksum = %02x\n",chksum); */
 chksum_count = (chksum_count - chksum);
 c1 = (chksum_count % 256);
 c2 = -c1;
 if (c2 != chksum)
   {
    printf(" Checksum %02x unequal to computed checksum %02x\n",
           chksum, c2);
   }
 chksum_count = 0;
 return;
}

/******************************
* gather up an unsized string into chr_buff *
******************************/
void get_u_string(num_chars)
byte num_chars;
{
byte get_obj_byte(void);

 for(i=0; i < num_chars; i++)
  {
   c = get_obj_byte();
   if ( isprint(c) )
    chr_buff[i] = c;
   else
    chr_buff[i] = BLANK;
  }
 chr_buff[i] = '\0';
 return;
}

/***************************
* gather up a sized string into chr_buff *
***************************/
byte get_s_string()
{
void get_u_string(byte);
byte get_obj_byte(void);

 num_bytes = get_obj_byte();
 get_u_string(num_bytes);
 return(num_bytes);
}

/******************************
* get index value (1 or 2 bytes) *
******************************/
byte get_index(index)
word *index;
{
byte get_obj_byte(void);

 c = get_obj_byte();
 if (c & BIT7)
  {
   c1 = (c & ~BIT7);
   c2 = get_obj_byte();
   *index = (c1 << 8) + c2;
   return(sizeof(word));
  }
 else
  {
   *index = c;
   return(sizeof(byte));
  }
/* return; */
}

/***************************************************
* Get a COMDEF length, 1 2 3 4 or 5 bytes.         *
***************************************************/
byte get_varlength(tolong)
long *tolong;
{
byte numform, used, toget, tozero;
byte *zapper;

        zapper = (char *)tolong;
        numform = get_obj_byte();
        used = 1;
        if (numform < 128)
        {
                *zapper++ = numform;
                toget = 0;
                tozero = 3;
        }
        else if (numform == 129)
        {
                toget = 2;
                tozero = 2;
        }
        else if (numform == 132)
        {
                toget = 3;
                tozero = 1;
        }
        else /* four-byte number */
        {
                toget = 4;
                tozero = 0;
        }
        for (; toget; --toget, ++used)
                *zapper++ = get_obj_byte();
        for (; tozero; --tozero)
                *zapper++ = 0;
        return(used);
}

/*********************************************************
* print out an Lname if there is one of the given index  *
**********************************************************/
void put_name(inx)
word inx;
{
    if (inx)
        if (inx < Lnames_index)
            printf("%s\n",Lnames[inx]);
        else
            printf("???\n");
    else
        printf("{nul}\n");
}

/***********************************************
* dump a given number of record bytes in hex   *
***********************************************/
void dumpn(cnt)
word cnt;
{
byte b[16];
word adr, p, q;
    adr = 0;
    while ( (adr < cnt) && (stoppit == 0) )
    {
        q = cnt-adr; if (q > 16) q = 16;
        for (p=0; p<q; p++)
            b[p] = get_obj_byte();
        printf("%04x  ",adr);
        for (p=0; p<q; p++)
            printf("%02x ",b[p]);
        for (; p<16; p++)
            printf("   ");
        for (p=0; p<q; p++)
            printf("%c", ((31<b[p])&&(b[p]<127))?b[p]:'.');
        printf("\n");
        adr += q;
    }
}
/**************************
* print out THEADR record *
**************************/
void do_theadr()
{
void do_checksum(void);
byte get_s_string(void);
word get_reclen(void);

 printf("\n80h: THEADR");
 reclen = get_reclen();
 get_s_string();
 printf("\tmodule = %s\n",chr_buff);
 do_checksum();
 return;
}

/**************************
* print out COMENT record *
* -including support for OS/2 dynalink record class A0h *
**************************/
void do_coment()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);
word get_obj_word(byte);
void get_u_string(byte);
byte get_s_string(void);
byte purge,list,class;
word ordinal;

 printf("88h: COMENT");
 reclen = get_reclen() - 1;
 c1 = get_obj_byte();
 purge = (c1 & BIT7) >> 7;
 list = (c1 & BIT6) >> 6;
 class = get_obj_byte();
 printf("\tclass = %01x, purge = %01x, list = %01x\n",
                  class,        purge,         list);
 if (class != 0xa0)
 {
     num_bytes = reclen - 2; /* adust for flag, class */
     get_u_string(num_bytes);
     printf("\tComment = \"%s\"\n",chr_buff);
     switch (class)
     {
         case 0x00: printf("\t\tcompiler name, /dsalloc\n"); break;
         case 0x9c: printf("\t\tDOS version\n"); break;
         case 0x9d: printf("\t\tmemory model\n"); break;
         case 0x9e: printf("\t\tforce /dosseg order\n"); break;
         case 0x9f: printf("\t\tlibrary specifier\n"); break;
         case 0x81: printf("\t\tlibrary specifier (obs.)\n"); break;
         case 0xa1: printf("\t\tMSoft extension marker\n"); break;
     }
 }
 else /* dynamic link import record */
 {
        list = get_obj_byte();
        class = get_obj_byte();
        printf("\tdynalink import, flag1 %02x, flag2 %02x\n",list,class);
        num_bytes = get_obj_byte();
        get_u_string(num_bytes);
        printf("\tName = %s",chr_buff);
        num_bytes = get_obj_byte();
        get_u_string(num_bytes);
        printf(", Module = %s",chr_buff);
        if (class)
        {
            ordinal = get_obj_word(SWAP);
            printf(", Ordinal %d\n",ordinal);
        }
        else
        {
            num_bytes = get_obj_byte();
            get_u_string(num_bytes);
            printf(", Entry = %s\n",chr_buff);
        }
 }
 do_checksum();
 return;
}

/**************************
* print out LNAMES record *
**************************/
void do_lnames()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);
word sum,name_index;

    printf("96h: LNAMES");
    reclen = get_reclen();
    reclen = reclen - 1;  /* adjust for checksum byte */
    sum = 0;
    while (sum < reclen)
    {
        num_bytes = get_obj_byte();  /* get length of string */
        sum = sum + (num_bytes + 1); /* account for length */
        name_index = Lnames_index;  /* next free entry */
        if (name_index > MAX_LNAME_ENTRIES) /* no more free */
            name_index = 0; /* 0th entry always free */
        if (num_bytes != 0)   /* check for a null name */
        {
            for(i=0; i < num_bytes; i++)
                Lnames[name_index][i] = get_obj_byte();
        }
        else
        {
            Lnames[name_index][0] = '{';
            Lnames[name_index][1] = '}';
            i=2;
        }
        Lnames[name_index][i] = '\0';
        printf("\t%02x  %s", name_index, Lnames[name_index]);
        if (name_index != 0)
        {
            Lnames_index++;
            printf("\n");
        }
        else
            printf(" (no room for this name)\n");
    }
    Lnames_total = Lnames_index - 1;
    do_checksum();
return;
}

/**************************
* print out SEGDEF record *
**************************/
void do_segdef()
{
void do_checksum(void);
byte get_index(word *);
byte get_obj_byte(void);
word get_obj_word(byte);
word get_reclen(void);
byte align,combine,big;
word abs_fn,abs_off;
word seg_len;
word seg_index,class_index,ovl_index;

 printf("98h: SEGDEF");
 reclen = get_reclen();
 c = get_obj_byte();
 align = (c & (BIT7+BIT6+BIT5)) >> 5;
 combine = (c & (BIT4+BIT3+BIT2)) >> 2;
 big = (c & BIT1) >> 1;
 printf("\tAlignment = %s\n",align_msg[align]);
 if (align == 0)
  {
   abs_fn = get_obj_word(SWAP);
   abs_off = get_obj_word(SWAP);
   printf("\t\tFrame = %04x : Offset = %04x\n",
          abs_fn,abs_off);
  }
 printf("\tCombine = %s\n",combine_msg[combine]);
 seg_len = get_obj_word(SWAP);

 printf("\tSegment length =");
 if (big == 1)
   printf(" 64K\n");
 else
   printf(" %04x\n",seg_len);

 get_index(&seg_index);
 printf("\tSegment name = ");
 put_name(seg_index);

 get_index(&class_index);
 printf("\tSegment class = ");
 put_name(class_index);

 get_index(&ovl_index);
 printf("\tSegment overlay = ");
 put_name(ovl_index);

 if (Snames_index > MAX_SNAME_ENTRIES)
  {
   printf(" No room to store SNAME\n");
   Snames_index = MAX_SNAME_ENTRIES+1;
   Snames_total = MAX_SNAME_ENTRIES;
  }
 else
  {
   Snames[Snames_index][SNAME_SEG] = seg_index;
   Snames[Snames_index][SNAME_CLASS] = class_index;
   Snames[Snames_index][SNAME_OVL] = ovl_index;
   Snames_index++;
   Snames_total = (Snames_index - 1);
  }
 do_checksum();
 return;
}

/**************************
* print out GRPDEF record *
**************************/
void do_grpdef()
{
void do_checksum(void);
byte get_index(word *);
byte get_obj_byte(void);
word get_reclen(void);
word index;
word sum;

 printf("9Ah: GRPDEF");
 reclen = get_reclen();
 /* adjust reclen for checksum */
 reclen = reclen - 1;
 sum = get_index(&index);
 printf("\tGroup = "); put_name(index);
 while (sum < reclen)
 {
     c = get_obj_byte(); /* read the FFh */
     sum++;
     sum += get_index(&index);
     printf("\t\tsegment = ");
     put_name(Snames[index][SNAME_SEG]);
 }
 do_checksum();
 return;
}

/**************************
* print out EXTDEF record *
**************************/
void do_extdef()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);
void get_u_string(byte);
word sum;
word type_index;

 printf("8Ch: EXTDEF");
 reclen = get_reclen();
 reclen = reclen - 1;   /* adjust reclen for checksum byte */
 sum = 0;
 while (sum < reclen)
  {
   num_bytes = get_obj_byte();
   sum = sum + (num_bytes + 1);
   get_u_string(num_bytes);
   sum += get_index(&type_index);
   printf("\ttype %04x, name = %s\n",
          type_index,chr_buff);
  }
 do_checksum();
 return;
}

/**************************
* print out COMDEF record *
* defines communal (FORTRAN COMMON, any C public array) variable *
**************************/
void do_comdef()
{

void do_checksum(void);
byte get_obj_byte(void);
word get_obj_word(byte);
word get_reclen(void);
byte get_index(word *);
byte get_varlength(long *);
byte get_s_string(void);

byte seg_type;
word sum, type_index;
long num_elem, size_elem;

 printf("B0h: COMDEF");
 reclen = get_reclen() - 1;
 sum = 0;
 while (sum < reclen)
 {
        sum += get_s_string() + 1;
        sum += get_index(&type_index);
        seg_type = get_obj_byte(); sum++;
        if (seg_type == 0x61) /* FAR */
                sum += get_varlength(&num_elem);
        sum += get_varlength(&size_elem);
        printf("\ttype = %04x, segtype = %02x, name = %s\n",
                    type_index,       seg_type,     chr_buff);
        if (seg_type == 0x61)
                printf("\t\t %l items of length %l\n",
                          num_elem,        size_elem);
        else
                printf("\t\titem size %l\n",size_elem);
 }
 do_checksum();
 return;
}

/**************************
* print out LEDATA record *
**************************/
void do_ledata()
{
void do_checksum(void);
byte get_obj_byte(void);
byte get_index(word *);
word get_reclen(void);
word sum, index;

 printf("A0h: LEDATA");
 reclen = get_reclen() - 1;
 sum = get_index(&index);
 printf("\tSegment = "); put_name(Snames[index][SNAME_SEG]);
 i = get_obj_word(SWAP);
 sum += 2;
 printf("\tEnumerated data offset = %04x\n",i);
 dumpn(reclen - sum);
 do_checksum();
 return;
}

/**************************
* print out BLKDEF record *
**************************/
void do_blkdef()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);

 printf("7Ah: BLKDEF");
 reclen = get_reclen();
 dumpn(reclen-1);
 do_checksum();
 return;
}

/**************************
* print out BLKEND record *
**************************/
void do_blkend()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);

 printf("7Ch: BLKEND");
 reclen = get_reclen();
 dumpn(reclen-1);
 do_checksum();
 return;
}

/**************************
* print out MODEND record *
**************************/
void do_modend()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);
byte flags, ismain, hasadr;

 printf("8Ah: MODEND");
 reclen = get_reclen();
 flags = get_obj_byte();
 ismain = (flags >> 7)&0x01;
 hasadr = (flags >> 6)&0x01;
 printf("\tModType = %02x, main: %c, start address: %c\n",
                flags,
                ismain ? '1' : '0',
                hasadr ? '1' : '0');
 dumpn(reclen-2);
 do_checksum();
 return;
}

/**************************
* print out TYPDEF record *
**************************/
void do_typdef()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);

 printf("8Eh: TYPDEF");
 reclen = get_reclen();
 dumpn(reclen-1);
 do_checksum();
 return;
}

/**************************
* print out PUBDEF record *
**************************/
void do_pubdef()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);
word grp, seg, frame, type, sum;

 printf("90h: PUBDEF");
 reclen = get_reclen();
 reclen -= 1; /* adjust for checksum */
 sum = get_index(&grp);
 printf("\tGroup ="); put_name(grp);
 sum += get_index(&seg);
 if (seg)
 {
     printf("\tSegment = ");
     put_name(Snames[seg][SNAME_SEG]);
 }
 else
 {
     i = get_obj_word(SWAP);
     sum += 2;
     printf("\tSegment = 0, Frame = %04x\n",i);
 }
 while (sum < reclen)
 {
     sum += get_s_string() + 1;
     i = get_obj_word(SWAP);
     sum += 2;
     sum += get_index(&type);
     printf("\t\ttype %04x, offset %04x, name = %s\n",
                        type,         i,        chr_buff);
 }
 do_checksum();
 return;
}

/**************************
* print out LOCSYM record (identical to PUBDEF) *
**************************/
void do_locsym()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);
word grp, seg, frame, type, sum;

 printf("92h: LOCSYM");
 reclen = get_reclen();
 reclen -= 1; /* adjust for checksum */
 sum = get_index(&grp);
 printf("\tGroup ="); put_name(grp);
 sum += get_index(&seg);
 if (seg)
 {
     printf("\tSegment = ");
     put_name(Snames[seg][SNAME_SEG]);
 }
 else
 {
     i = get_obj_word(SWAP);
     sum += 2;
     printf("\tSegment = 0, Frame = %04x\n",i);
 }
 while (sum < reclen)
 {
     sum += get_s_string() + 1;
     i = get_obj_word(SWAP);
     sum += 2;
     sum += get_index(&type);
     printf("\t\ttype %04x, offset %04x, name = %s\n",
                        type,         i,        chr_buff);
 }
 do_checksum();
 return;
}

/**************************
* print out LINNUM record *
**************************/
void do_linnum()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);
word grp, seg, number, offset, sum;

 printf("94h: LINNUM");
 reclen = get_reclen() - 1;
 sum = get_index(&grp);
 printf("\tGroup = "); put_name(grp);
 sum += get_index(&seg);
 if (seg)
 {
     printf("\tSegment = ");
     put_name(Snames[seg][SNAME_SEG]);
 }
 else printf("\tSegment = 0\n");
 while (sum < reclen)
 {
     number = get_obj_word(SWAP);
     offset = get_obj_word(SWAP);
     sum += 4;
     printf("\t\tNumber = %04x, offset = %04x\n",number,offset);
 }
 do_checksum();
 return;
}

/**************************
* print out FIXUPP record *
**************************/
void do_fixupp()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);

 printf("9Ch: FIXUPP");
 reclen = get_reclen();
 dumpn(reclen - 1);
 do_checksum();
 return;
}

/**************************
* print out LIDATA record *
**************************/
void do_lidata()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);

 printf("A2h: LIDATA");
 reclen = get_reclen();
 dumpn(reclen - 1);
 do_checksum();
 return;
}

/*********************************
* unimplemented record type trap *
*********************************/
void do_unimplt()
{
void do_checksum(void);
byte get_obj_byte(void);
word get_reclen(void);
word dumplen;

 printf("%02xh: unknown record type",c);
 dumplen = (reclen = get_reclen()) - 1;
 dumpn(dumplen);
 do_checksum();
 return;
}
                                                                                                                                                                                                           