%=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
%             _             _
%            | |           | |
%  _ __ ___  | |__    ___  | | ___   ___   _ __       ___   ___   _ __ ___
% | '_ ` _ \ | '_ \  / _ \ | |/ __| / _ \ | '_ \     / __| / _ \ | '_ ` _ \
% | | | | | || | | || (_) || |\__ \| (_) || | | | _ | (__ | (_) || | | | | |
% |_| |_| |_||_| |_| \___/ |_||___/ \___/ |_| |_|(_) \___| \___/ |_| |_| |_|
%
% Author: Mark H. Olson
% Website: https://mholson.com
% Github: https://github.com/mholson
%
% Created: 2021-06-09
%
% > > > This file is based on 
% > > > This is file `maatext.cls',
% > > > generated with the docstrip utility.
% > > >
% > > > The original source files were:
% > > >
% > > > maatext.dtx 
% > > > This is a generated file.
% > > > 
% > > > Copyright 2018 American Mathematical Society.
% > > > 
% > > > American Mathematical Society
% > > > Technical Support
% > > > Publications Technical Group
% > > > 201 Charles Street
% > > > Providence, RI 02904
% > > > USA
% > > > tel: (401) 455-4080
% > > >      (800) 321-4267 (USA and Canada only)
% > > > fax: (401) 331-3842
% > > > email: tech-support@ams.org
% > > > 
% > > > This work may be distributed and/or modified under the
% > > > conditions of the LaTeX Project Public License, either version 1.3c
% > > > of this license or (at your option) any later version.
% > > > The latest version of this license is in
% > > >   http://www.latex-project.org/lppl.txt
% > > > and version 1.3c or later is part of all distributions of LaTeX
% > > > version 2005/12/01 or later.
% > > > 
% > > > This work has the LPPL maintenance status `maintained'.
% > > > 
% > > > The Current Maintainer of this work is the American Mathematical
% > > > Society.
%
%  This template is designed for International Baccalaureate students who
%  are looking to write an extended essary or internal assessment using LaTeX.
%  It is based on the MAA monograph AMS/MAA textbook template
%  (http://www.ams.org/arc/books/book-produce.html#maamono). Some modifications
%  have been made to the original class files (maatext.cls and maabook.cls) to
%  better meet the requirements of the International Baccalaureate and these
%  modifications are maintained at 
%  (https://github.com/mholson/mhoTexmf/tree/main/texmf).  
%=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=

\NeedsTeXFormat{LaTeX2e}[2016/03/31]
\ProvidesClass{maatext}[2018/07/16 v1.02]
\RequirePackage{xcolor}
\RequirePackage{mhocolorthemenord}
\RequirePackage{textcomp}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{maabook}}
\ProcessOptions\relax
\LoadClass[7x10]{maabook}[2018/07/16]

\fm@def\wordcount{\def\@wordcount}
\let\@wordcount\@empty
\fm@def\subcategory{\def\@subcategory}
\let\@subcategory\@empty
\fm@def\subsubject{\def\@subsubject}
\let\@subsubject\@empty

\def\set@wordcount#1{%
    #1\par
}

\def\titlepage@series{Textbooks}

\def\@makechapterhead#1{%
    \begingroup
        \null
        \vskip 21bp % Fudge factor
        \fontfamily{\sfdefault}%
        \fontsize{24}{27.5}%
        \color{nordZero}
        \bfseries
        \raggedleft
        \ifnum \c@secnumdepth >\m@ne
            \ifappendix
                \begingroup
                    \fontsize{18}{18}\selectfont
                    \color{nordTen}%
                    \appendixname\space
                \endgroup
            \fi
            \begingroup
                \normalfont
                \bfseries
                \fontsize{50}{50}\selectfont
                \color{nordTen}%
                \thechapter\par
            \endgroup
        \fi
        \bb@space 3.5 pc
        #1\par
    \endgroup
    \bb@space 4.5pc
}
\def\section{%
    \@startsection{section}%
        {1}%
        \z@
        {-3pc\@minus-0.6pc}%
        {.5pc}%
        {\color{nordOne}\normalfont\sffamily\bfseries\fontsize{16}{18pt}\selectfont}%
}

\def\maketitle@default{%
    \begin{titlepage}%
    \def\title@bookmark{Title page}%
    \@maketitle@hook
    \normalfont
    \sffamily
    \begingroup
        \begingroup
            \maa@seriesfontsize
            \begingroup
                \color{nordOne}\fontseries{bc}\selectfont\@subcategory
            \endgroup
            \quad
            {\color{nordZero}\vrule width 1pt height 12pt}
            \quad
            \begingroup
                \color{nordOne}\fontseries{c}\selectfont
                \@subsubject
                %\uppercase\expandafter{Math\kern -1pt \lower .7ex\hbox{s}\kern -1pt Notes}\par
            \endgroup
            \ifx\@volumeno\@empty\else
                \fontseries{c}\selectfont
                \bb@space 0.4375in
                VOL\enskip{\fontseries{bc}\selectfont\@volumeno}\par
            \fi
        \endgroup
        \bfseries
        \ifx\@title\@empty\else
            \begingroup
                \color{nordOne}\fontsize{26}{31.2}\selectfont
                \bb@space 1.3125in
                \@title\par
                \ifx\@subtitle\@empty\else
                    \color{nordZero}\fontseries{bc}\fontsize{14}{20}\selectfont
                    \bb@space 0.5in
                    \@subtitle\par
                \fi
            \endgroup
        \fi
    \endgroup
    \begingroup
        %\bfseries
        \color{nordOne}
        \fontsize{12}{18}\selectfont
        \bb@space 1in
        \let\@authorname\set@authorname
        \@authorlist\par
        \ifx\@wordcount\@empty\else
        \bb@space 1in
        word count: \@wordcount
        \fi
    \endgroup
    \vfill
    \maa@ams@logo
    \end{titlepage}%
    \ams@include@cip
}


\def\contentsname{\color{nordZero}Contents}
\def\listfigurename{\color{nordZero}List of Figures}
\def\listtablename{\color{nordZero}List of Tables}
\endinput
%%
%% End of file `maatext.cls'.
