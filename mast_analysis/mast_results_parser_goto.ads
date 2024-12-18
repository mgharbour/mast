-----------------------------------------------------------------------
--                              Mast                                 --
--     Modelling and Analysis Suite for Real-Time Applications       --
--                                                                   --
--                       Copyright (C) 2001-2024                     --
--                 Universidad de Cantabria, SPAIN                   --
--                                                                   --
-- Authors: Michael Gonzalez       mgh@unican.es                     --
--          Jose Javier Gutierrez  gutierjj@unican.es                --
--          Jose Carlos Palencia   palencij@unican.es                --
--          Jose Maria Drake       drakej@unican.es                  --
--          Julio Luis Medina      medinajl@unican.es                --
--                                                                   --
-- This program is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This program is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this program; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-----------------------------------------------------------------------
package Mast_Results_Parser_Goto is

   type Small_Integer is range -32_000 .. 32_000;

   type Goto_Entry is record
        Nonterm  : Small_Integer;
        Newstate : Small_Integer;
    end record;

   --pragma suppress(index_check);

   subtype Row is Integer range -1 .. Integer'Last;

   type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

   Goto_Matrix : constant Goto_Parse_Table :=
       ((-1,-1)  -- Dummy Entry.
-- State  0
,(-9, 8),(-8, 7),(-7, 6),(-6, 5)
,(-5, 4),(-4, 3),(-3, 2),(-2, 1)

-- State  1
,(-9, 8),(-8, 7),(-7, 6),(-6, 5)
,(-5, 4),(-4, 3),(-3, 16)
-- State  2

-- State  3

-- State  4

-- State  5

-- State  6

-- State  7

-- State  8

-- State  9

-- State  10

-- State  11

-- State  12

-- State  13

-- State  14

-- State  15

-- State  16

-- State  17
,(-17, 30)
,(-16, 29),(-15, 28),(-14, 27),(-13, 26)
,(-12, 25),(-11, 23),(-10, 24)
-- State  18
,(-27, 40)
,(-26, 39),(-25, 37),(-24, 38)
-- State  19
,(-49, 46)
,(-48, 45),(-47, 43),(-46, 44)
-- State  20
,(-63, 52)
,(-62, 51),(-61, 49),(-60, 50)
-- State  21
,(-128, 58)
,(-127, 57),(-126, 55),(-125, 56)
-- State  22
,(-187, 64)
,(-186, 63),(-185, 61),(-184, 62)
-- State  23

-- State  24

-- State  25

-- State  26

-- State  27

-- State  28

-- State  29

-- State  30

-- State  31

-- State  32

-- State  33

-- State  34

-- State  35

-- State  36

-- State  37

-- State  38

-- State  39

-- State  40

-- State  41

-- State  42

-- State  43

-- State  44

-- State  45

-- State  46

-- State  47

-- State  48

-- State  49

-- State  50

-- State  51

-- State  52

-- State  53

-- State  54

-- State  55

-- State  56

-- State  57

-- State  58

-- State  59

-- State  60

-- State  61

-- State  62

-- State  63

-- State  64

-- State  65

-- State  66

-- State  67

-- State  68
,(-17, 30)
,(-16, 29),(-15, 28),(-14, 27),(-13, 26)
,(-12, 25),(-11, 96)
-- State  69

-- State  70

-- State  71

-- State  72

-- State  73

-- State  74

-- State  75

-- State  76
,(-27, 40),(-26, 39)
,(-25, 104)
-- State  77

-- State  78

-- State  79

-- State  80
,(-49, 46),(-48, 45),(-47, 108)

-- State  81

-- State  82

-- State  83

-- State  84
,(-63, 52),(-62, 51),(-61, 112)
-- State  85

-- State  86

-- State  87

-- State  88
,(-128, 58)
,(-127, 57),(-126, 116)
-- State  89

-- State  90

-- State  91

-- State  92
,(-187, 64),(-186, 63)
,(-185, 120)
-- State  93

-- State  94

-- State  95

-- State  96

-- State  97

-- State  98

-- State  99

-- State  100

-- State  101

-- State  102
,(-21, 126),(-20, 125),(-19, 124)
,(-18, 123)
-- State  103

-- State  104

-- State  105

-- State  106
,(-35, 135),(-34, 134),(-32, 132)
,(-31, 131),(-30, 130),(-29, 129),(-28, 128)

-- State  107

-- State  108

-- State  109

-- State  110
,(-55, 141),(-54, 140),(-53, 139),(-52, 138)
,(-51, 137),(-50, 136)
-- State  111

-- State  112

-- State  113

-- State  114
,(-66, 145),(-65, 144)
,(-64, 143)
-- State  115

-- State  116

-- State  117

-- State  118
,(-133, 151),(-132, 150),(-131, 149)
,(-130, 148),(-129, 147)
-- State  119

-- State  120

-- State  121

-- State  122
,(-191, 156),(-190, 155)
,(-189, 154),(-188, 153)
-- State  123

-- State  124

-- State  125

-- State  126

-- State  127

-- State  128

-- State  129

-- State  130

-- State  131

-- State  132

-- State  133

-- State  134

-- State  135

-- State  136

-- State  137

-- State  138

-- State  139

-- State  140

-- State  141

-- State  142

-- State  143

-- State  144

-- State  145

-- State  146

-- State  147

-- State  148

-- State  149

-- State  150

-- State  151

-- State  152

-- State  153

-- State  154

-- State  155

-- State  156

-- State  157

-- State  158

-- State  159
,(-21, 126),(-20, 125)
,(-19, 176)
-- State  160

-- State  161

-- State  162
,(-35, 135),(-34, 134),(-32, 132)
,(-31, 131),(-30, 130),(-29, 178)
-- State  163

-- State  164

-- State  165
,(-55, 141)
,(-54, 140),(-53, 139),(-52, 138),(-51, 180)

-- State  166

-- State  167

-- State  168
,(-66, 145),(-65, 182)
-- State  169

-- State  170

-- State  171
,(-133, 151),(-132, 150)
,(-131, 149),(-130, 184)
-- State  172

-- State  173

-- State  174
,(-191, 156),(-190, 155)
,(-189, 186)
-- State  175

-- State  176

-- State  177

-- State  178

-- State  179

-- State  180

-- State  181

-- State  182

-- State  183

-- State  184

-- State  185

-- State  186

-- State  187

-- State  188

-- State  189

-- State  190

-- State  191

-- State  192

-- State  193

-- State  194

-- State  195

-- State  196

-- State  197

-- State  198

-- State  199

-- State  200

-- State  201

-- State  202

-- State  203

-- State  204
,(-22, 220)
-- State  205
,(-23, 221)
-- State  206
,(-33, 222)

-- State  207
,(-36, 223)
-- State  208
,(-38, 224)
-- State  209
,(-45, 225)
-- State  210
,(-56, 226)

-- State  211
,(-57, 227)
-- State  212
,(-58, 228)
-- State  213
,(-59, 229)
-- State  214
,(-67, 230)

-- State  215
,(-134, 231)
-- State  216
,(-135, 232)
-- State  217
,(-159, 233)
-- State  218
,(-192, 234)

-- State  219
,(-193, 235)
-- State  220

-- State  221

-- State  222

-- State  223
,(-37, 240)
-- State  224
,(-44, 247),(-43, 246)
,(-42, 245),(-41, 244),(-40, 241),(-39, 242)
,(-37, 243)
-- State  225

-- State  226

-- State  227

-- State  228

-- State  229

-- State  230

-- State  231

-- State  232
,(-145, 268),(-144, 267),(-143, 266)
,(-142, 265),(-141, 264),(-140, 263),(-139, 262)
,(-138, 261),(-137, 259),(-136, 260)
-- State  233
,(-169, 294)
,(-168, 293),(-167, 290),(-166, 288),(-165, 287)
,(-164, 286),(-163, 284),(-162, 281),(-161, 277)
,(-160, 278),(-145, 292),(-144, 291),(-143, 289)
,(-142, 285),(-141, 283),(-140, 282),(-139, 280)
,(-138, 279)
-- State  234

-- State  235

-- State  236

-- State  237

-- State  238

-- State  239

-- State  240

-- State  241

-- State  242

-- State  243

-- State  244

-- State  245

-- State  246

-- State  247

-- State  248

-- State  249

-- State  250

-- State  251

-- State  252

-- State  253

-- State  254

-- State  255

-- State  256

-- State  257

-- State  258

-- State  259

-- State  260

-- State  261

-- State  262

-- State  263

-- State  264

-- State  265

-- State  266

-- State  267

-- State  268

-- State  269

-- State  270

-- State  271

-- State  272

-- State  273

-- State  274

-- State  275

-- State  276

-- State  277

-- State  278

-- State  279

-- State  280

-- State  281

-- State  282

-- State  283

-- State  284

-- State  285

-- State  286

-- State  287

-- State  288

-- State  289

-- State  290

-- State  291

-- State  292

-- State  293

-- State  294

-- State  295

-- State  296

-- State  297

-- State  298

-- State  299

-- State  300

-- State  301

-- State  302

-- State  303

-- State  304

-- State  305

-- State  306

-- State  307

-- State  308

-- State  309

-- State  310

-- State  311
,(-44, 247),(-43, 246),(-42, 245)
,(-41, 244),(-40, 349),(-37, 243)
-- State  312

-- State  313

-- State  314

-- State  315

-- State  316

-- State  317

-- State  318

-- State  319

-- State  320

-- State  321

-- State  322

-- State  323

-- State  324
,(-145, 268)
,(-144, 267),(-143, 266),(-142, 265),(-141, 264)
,(-140, 263),(-139, 262),(-138, 261),(-137, 361)

-- State  325

-- State  326

-- State  327

-- State  328

-- State  329

-- State  330

-- State  331

-- State  332

-- State  333

-- State  334
,(-169, 294),(-168, 293),(-167, 290),(-166, 288)
,(-165, 287),(-164, 286),(-163, 284),(-162, 281)
,(-161, 370),(-145, 292),(-144, 291),(-143, 289)
,(-142, 285),(-141, 283),(-140, 282),(-139, 280)
,(-138, 279)
-- State  335

-- State  336

-- State  337

-- State  338

-- State  339

-- State  340

-- State  341

-- State  342

-- State  343
,(-74, 384),(-73, 383),(-72, 382)
,(-71, 381),(-70, 380),(-69, 379),(-68, 386)

-- State  344
,(-117, 387),(-116, 389)
-- State  345

-- State  346

-- State  347

-- State  348

-- State  349

-- State  350

-- State  351

-- State  352

-- State  353

-- State  354

-- State  355

-- State  356

-- State  357

-- State  358

-- State  359

-- State  360

-- State  361

-- State  362

-- State  363

-- State  364

-- State  365

-- State  366

-- State  367
,(-147, 400),(-146, 401)

-- State  368
,(-152, 403),(-151, 404)
-- State  369
,(-156, 406),(-155, 407)

-- State  370

-- State  371

-- State  372

-- State  373

-- State  374

-- State  375

-- State  376
,(-171, 409),(-170, 410)
-- State  377
,(-175, 412),(-174, 413)

-- State  378
,(-179, 415),(-178, 416)
-- State  379

-- State  380

-- State  381

-- State  382

-- State  383

-- State  384

-- State  385
,(-109, 424),(-100, 423)
,(-92, 422),(-87, 421),(-82, 420),(-75, 419)

-- State  386

-- State  387

-- State  388
,(-118, 427)
-- State  389

-- State  390

-- State  391

-- State  392

-- State  393

-- State  394

-- State  395

-- State  396

-- State  397

-- State  398

-- State  399

-- State  400

-- State  401

-- State  402
,(-149, 431),(-148, 433)
-- State  403

-- State  404

-- State  405
,(-153, 437)
,(-149, 436)
-- State  406

-- State  407

-- State  408
,(-157, 441),(-149, 440)
-- State  409

-- State  410

-- State  411
,(-172, 445)
,(-149, 444)
-- State  412

-- State  413

-- State  414
,(-176, 449)
-- State  415

-- State  416

-- State  417
,(-149, 452)
-- State  418

-- State  419
,(-76, 454)

-- State  420
,(-83, 455)
-- State  421
,(-88, 456)
-- State  422
,(-93, 457)
-- State  423
,(-101, 458)

-- State  424
,(-110, 459)
-- State  425

-- State  426

-- State  427
,(-119, 461)
-- State  428

-- State  429

-- State  430
,(-147, 462)
-- State  431

-- State  432

-- State  433

-- State  434

-- State  435
,(-152, 466)

-- State  436

-- State  437

-- State  438

-- State  439
,(-156, 469)
-- State  440

-- State  441

-- State  442

-- State  443
,(-171, 472)
-- State  444

-- State  445

-- State  446

-- State  447
,(-175, 475)
-- State  448

-- State  449

-- State  450

-- State  451
,(-179, 478)

-- State  452

-- State  453

-- State  454
,(-81, 490),(-80, 489),(-79, 488),(-78, 487)
,(-77, 492)
-- State  455
,(-86, 495),(-85, 494),(-84, 498)
,(-81, 497),(-80, 496)
-- State  456
,(-91, 501),(-90, 500)
,(-89, 504),(-81, 503),(-80, 502)
-- State  457
,(-99, 512)
,(-98, 511),(-97, 510),(-96, 509),(-95, 505)
,(-94, 506),(-81, 508),(-80, 507)
-- State  458
,(-108, 521)
,(-107, 520),(-106, 519),(-105, 518),(-104, 516)
,(-103, 514),(-102, 515),(-81, 517)
-- State  459
,(-115, 527)
,(-114, 526),(-113, 525),(-112, 524),(-111, 529)

-- State  460

-- State  461
,(-124, 535),(-123, 534),(-122, 533),(-121, 532)
,(-120, 537)
-- State  462

-- State  463
,(-150, 539)
-- State  464

-- State  465

-- State  466

-- State  467
,(-154, 542)
-- State  468

-- State  469

-- State  470
,(-158, 544)

-- State  471

-- State  472

-- State  473
,(-173, 546)
-- State  474

-- State  475

-- State  476

-- State  477
,(-177, 549)
-- State  478

-- State  479
,(-180, 551)
-- State  480

-- State  481

-- State  482

-- State  483

-- State  484

-- State  485

-- State  486

-- State  487
,(-81, 490)
,(-80, 489),(-79, 553)
-- State  488

-- State  489

-- State  490

-- State  491

-- State  492

-- State  493

-- State  494
,(-86, 557),(-81, 497)
,(-80, 496)
-- State  495

-- State  496

-- State  497

-- State  498

-- State  499

-- State  500
,(-91, 559),(-81, 503),(-80, 502)

-- State  501

-- State  502

-- State  503

-- State  504

-- State  505

-- State  506
,(-99, 512),(-98, 511),(-97, 510),(-96, 509)
,(-95, 561),(-81, 508),(-80, 507)
-- State  507

-- State  508

-- State  509

-- State  510

-- State  511

-- State  512

-- State  513

-- State  514

-- State  515
,(-108, 521)
,(-107, 520),(-106, 519),(-105, 518),(-104, 516)
,(-103, 567),(-81, 517)
-- State  516

-- State  517

-- State  518

-- State  519

-- State  520

-- State  521

-- State  522

-- State  523

-- State  524
,(-115, 527),(-114, 526)
,(-113, 574)
-- State  525

-- State  526

-- State  527

-- State  528

-- State  529

-- State  530

-- State  531

-- State  532
,(-124, 535),(-123, 534),(-122, 578)

-- State  533

-- State  534

-- State  535

-- State  536

-- State  537

-- State  538

-- State  539

-- State  540

-- State  541

-- State  542

-- State  543

-- State  544

-- State  545

-- State  546

-- State  547

-- State  548

-- State  549

-- State  550

-- State  551

-- State  552

-- State  553

-- State  554

-- State  555

-- State  556

-- State  557

-- State  558

-- State  559

-- State  560

-- State  561

-- State  562

-- State  563

-- State  564

-- State  565

-- State  566

-- State  567

-- State  568

-- State  569

-- State  570

-- State  571

-- State  572

-- State  573

-- State  574

-- State  575

-- State  576

-- State  577

-- State  578

-- State  579

-- State  580

-- State  581

-- State  582

-- State  583

-- State  584

-- State  585

-- State  586

-- State  587

-- State  588

-- State  589

-- State  590

-- State  591

-- State  592

-- State  593

-- State  594

-- State  595

-- State  596

-- State  597

-- State  598

-- State  599

-- State  600

-- State  601

-- State  602

-- State  603

-- State  604

-- State  605

-- State  606

-- State  607

-- State  608

-- State  609
,(-182, 625),(-181, 626)
-- State  610

-- State  611

-- State  612

-- State  613

-- State  614

-- State  615

-- State  616

-- State  617

-- State  618

-- State  619

-- State  620

-- State  621

-- State  622

-- State  623

-- State  624

-- State  625

-- State  626

-- State  627
,(-176, 630)
-- State  628

-- State  629
,(-182, 631)

-- State  630

-- State  631

-- State  632
,(-183, 634)
-- State  633

-- State  634

-- State  635

-- State  636

-- State  637

);
--  The offset vector
GOTO_OFFSET : array (0.. 637) of Integer :=
( 0,
 8, 15, 15, 15, 15, 15, 15, 15, 15, 15,
 15, 15, 15, 15, 15, 15, 15, 23, 27, 31,
 35, 39, 43, 43, 43, 43, 43, 43, 43, 43,
 43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
 43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
 43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
 43, 43, 43, 43, 43, 43, 43, 43, 50, 50,
 50, 50, 50, 50, 50, 50, 53, 53, 53, 53,
 56, 56, 56, 56, 59, 59, 59, 59, 62, 62,
 62, 62, 65, 65, 65, 65, 65, 65, 65, 65,
 65, 65, 69, 69, 69, 69, 76, 76, 76, 76,
 82, 82, 82, 82, 85, 85, 85, 85, 90, 90,
 90, 90, 94, 94, 94, 94, 94, 94, 94, 94,
 94, 94, 94, 94, 94, 94, 94, 94, 94, 94,
 94, 94, 94, 94, 94, 94, 94, 94, 94, 94,
 94, 94, 94, 94, 94, 94, 94, 94, 94, 97,
 97, 97, 103, 103, 103, 108, 108, 108, 110, 110,
 110, 114, 114, 114, 117, 117, 117, 117, 117, 117,
 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,
 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,
 117, 117, 117, 117, 118, 119, 120, 121, 122, 123,
 124, 125, 126, 127, 128, 129, 130, 131, 132, 133,
 133, 133, 133, 134, 141, 141, 141, 141, 141, 141,
 141, 141, 151, 169, 169, 169, 169, 169, 169, 169,
 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
 169, 175, 175, 175, 175, 175, 175, 175, 175, 175,
 175, 175, 175, 175, 184, 184, 184, 184, 184, 184,
 184, 184, 184, 184, 201, 201, 201, 201, 201, 201,
 201, 201, 201, 208, 210, 210, 210, 210, 210, 210,
 210, 210, 210, 210, 210, 210, 210, 210, 210, 210,
 210, 210, 210, 210, 210, 210, 210, 212, 214, 216,
 216, 216, 216, 216, 216, 216, 218, 220, 222, 222,
 222, 222, 222, 222, 222, 228, 228, 228, 229, 229,
 229, 229, 229, 229, 229, 229, 229, 229, 229, 229,
 229, 229, 231, 231, 231, 233, 233, 233, 235, 235,
 235, 237, 237, 237, 238, 238, 238, 239, 239, 240,
 241, 242, 243, 244, 245, 245, 245, 246, 246, 246,
 247, 247, 247, 247, 247, 248, 248, 248, 248, 249,
 249, 249, 249, 250, 250, 250, 250, 251, 251, 251,
 251, 252, 252, 252, 257, 262, 267, 275, 283, 288,
 288, 293, 293, 294, 294, 294, 294, 295, 295, 295,
 296, 296, 296, 297, 297, 297, 297, 298, 298, 299,
 299, 299, 299, 299, 299, 299, 299, 302, 302, 302,
 302, 302, 302, 302, 305, 305, 305, 305, 305, 305,
 308, 308, 308, 308, 308, 308, 315, 315, 315, 315,
 315, 315, 315, 315, 315, 322, 322, 322, 322, 322,
 322, 322, 322, 322, 325, 325, 325, 325, 325, 325,
 325, 325, 328, 328, 328, 328, 328, 328, 328, 328,
 328, 328, 328, 328, 328, 328, 328, 328, 328, 328,
 328, 328, 328, 328, 328, 328, 328, 328, 328, 328,
 328, 328, 328, 328, 328, 328, 328, 328, 328, 328,
 328, 328, 328, 328, 328, 328, 328, 328, 328, 328,
 328, 328, 328, 328, 328, 328, 328, 328, 328, 328,
 328, 328, 328, 328, 328, 328, 328, 328, 328, 328,
 328, 328, 328, 328, 328, 328, 328, 328, 328, 330,
 330, 330, 330, 330, 330, 330, 330, 330, 330, 330,
 330, 330, 330, 330, 330, 330, 330, 331, 331, 332,
 332, 332, 333, 333, 333, 333, 333);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  298) of Natural := ( 2,
 2, 1, 1, 1, 1, 1, 1, 1,
 5, 1, 3, 1, 1, 1, 1, 1,
 1, 3, 3, 3, 3, 3, 5, 3,
 1, 1, 1, 0, 10, 0, 10, 5,
 1, 3, 1, 1, 3, 5, 3, 1,
 1, 1, 1, 0, 10, 1, 1, 0,
 8, 0, 8, 1, 3, 1, 1, 1,
 1, 1, 3, 3, 3, 3, 3, 0,
 10, 5, 1, 3, 1, 1, 3, 5,
 3, 1, 1, 1, 1, 1, 0, 10,
 0, 10, 0, 10, 0, 10, 5, 1,
 3, 1, 1, 3, 5, 3, 1, 1,
 0, 10, 1, 1, 1, 1, 1, 1,
 0, 4, 3, 1, 2, 1, 2, 1,
 1, 4, 4, 0, 4, 3, 1, 2,
 1, 2, 1, 1, 0, 4, 3, 1,
 2, 1, 2, 1, 1, 0, 5, 3,
 1, 2, 1, 1, 1, 1, 1, 1,
 4, 4, 4, 4, 0, 5, 3, 1,
 2, 1, 1, 1, 1, 1, 1, 4,
 4, 4, 4, 4, 0, 4, 3, 1,
 2, 1, 2, 1, 1, 4, 4, 1,
 0, 4, 3, 1, 2, 1, 2, 1,
 1, 4, 4, 5, 1, 3, 1, 1,
 3, 5, 3, 1, 1, 1, 1, 0,
 10, 0, 8, 1, 3, 1, 1, 1,
 1, 1, 1, 1, 1, 3, 3, 3,
 3, 3, 5, 1, 3, 3, 3, 3,
 3, 5, 1, 3, 3, 3, 3, 5,
 1, 3, 3, 3, 3, 0, 8, 1,
 3, 1, 1, 1, 1, 1, 1, 1,
 1, 1, 1, 1, 1, 1, 1, 1,
 1, 3, 3, 3, 3, 3, 5, 1,
 3, 3, 3, 3, 5, 1, 3, 5,
 3, 3, 5, 1, 3, 5, 5, 1,
 3, 5, 3, 5, 1, 3, 1, 1,
 3, 5, 3, 1, 1, 1, 0, 10,
 0, 10);
   Get_LHS_Rule: array (Rule range  0 ..  298) of Nonterminal := (-1,
-2,-2,-3,-3,-3,-3,-3,-3,
-4,-10,-10,-11,-11,-11,-11,-11,
-11,-12,-13,-14,-15,-16,-17,-18,
-18,-19,-19,-22,-20,-23,-21,-5,
-24,-24,-25,-25,-26,-27,-28,-28,
-29,-29,-29,-33,-30,-31,-31,-36,
-35,-38,-34,-39,-39,-40,-40,-40,
-40,-40,-37,-41,-42,-43,-44,-45,
-32,-6,-46,-46,-47,-47,-48,-49,
-50,-50,-51,-51,-51,-51,-56,-52,
-57,-53,-58,-54,-59,-55,-7,-60,
-60,-61,-61,-62,-63,-64,-64,-65,
-67,-66,-68,-68,-68,-68,-68,-68,
-76,-69,-75,-77,-77,-78,-78,-79,
-79,-81,-80,-83,-70,-82,-84,-84,
-85,-85,-86,-86,-88,-71,-87,-89,
-89,-90,-90,-91,-91,-93,-72,-92,
-94,-94,-95,-95,-95,-95,-95,-95,
-96,-97,-98,-99,-101,-73,-100,-102,
-102,-103,-103,-103,-103,-103,-103,-104,
-105,-106,-107,-108,-110,-74,-109,-111,
-111,-112,-112,-113,-113,-115,-114,-116,
-119,-117,-118,-120,-120,-121,-121,-122,
-122,-124,-123,-8,-125,-125,-126,-126,
-127,-128,-129,-129,-130,-130,-130,-134,
-131,-135,-132,-136,-136,-137,-137,-137,
-137,-137,-137,-137,-137,-138,-139,-140,
-141,-142,-143,-146,-146,-147,-148,-150,
-149,-144,-151,-151,-152,-153,-154,-145,
-155,-155,-156,-157,-158,-159,-133,-160,
-160,-161,-161,-161,-161,-161,-161,-161,
-161,-161,-161,-161,-161,-161,-161,-161,
-161,-162,-163,-164,-165,-166,-167,-170,
-170,-171,-172,-173,-168,-174,-174,-175,
-176,-177,-169,-178,-178,-179,-180,-181,
-181,-182,-183,-9,-184,-184,-185,-185,
-186,-187,-188,-188,-189,-189,-192,-190,
-193,-191);
end Mast_Results_Parser_Goto;
