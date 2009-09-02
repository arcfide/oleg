; Unicodes for HTML character entities
; by Mikael More
; Tue, 30 Sep 2008 13:12:34 +0200
;
; The following table could be uses as an extension or replacement
; for ssax:predefined-parsed-entities. It requires the function
; integer->char that accepts integer codes larger than 255.

; ## Custom addition:
; based on name2codepoint of python's htmlentitydefs library. To get it,
; type python, import htmlentitydefs, htmlentitydefs.name2codepoint

(define html-entity-unicode-numbers 
  '((zwnj . 8204) (aring . 229) (gt . 62) (yen . 165) (ograve . 242) (Chi . 935)
    (delta . 948) (rang . 9002) (sup . 8835) (trade . 8482) (Ntilde . 209) 
    (xi . 958) (upsih . 978) (Yacute . 221) (Atilde . 195) (radic . 8730) 
    (otimes . 8855) (aelig . 230) (oelig . 339) (equiv . 8801) (ni . 8715)
    (Psi . 936) (auml . 228) (Uuml . 220) (Epsilon . 917) (Yuml . 376) 
    (lt . 60) (Icirc . 206) (shy . 173) (Upsilon . 933) (Lambda . 923)
    (yacute . 253) (Prime . 8243) (prime . 8242) (psi . 968) (Kappa . 922)
    (rsaquo . 8250) (Tau . 932) (darr . 8595) (ocirc . 244) (lrm . 8206)
    (zwj . 8205) (cedil . 184) (rlm . 8207) (Alpha . 913) (not . 172)
    (amp . 38) (AElig . 198) (oslash . 248) (acute . 180) (lceil . 8968)
    (iquest . 191) (uacute . 250) (laquo . 171) (dArr . 8659)
    (rdquo . 8221) (ge . 8805) (Igrave . 204) (nu . 957) (ccedil . 231)
    (lsaquo . 8249) (sube . 8838) (rarr . 8594) (sdot . 8901) (supe . 8839)
    (nbsp . 160) (lfloor . 8970) (lArr . 8656) (Auml . 196)
    (asymp . 8776) (Otilde . 213) (szlig . 223) (clubs . 9827) (agrave . 224)
    (Ocirc . 212) (ndash . 8211) (Theta . 920) (Pi . 928) (OElig . 338)
    (Scaron . 352) (frac14 . 188) (egrave . 232) (sub . 8834) (iexcl . 161)
    (frac12 . 189) (ordf . 170) (sum . 8721) (prop . 8733) (circ . 710)
    (ntilde . 241) (atilde . 227) (theta . 952) (prod . 8719) (nsub . 8836)
    (hArr . 8660) (rArr . 8658) (Oslash . 216) (emsp . 8195) (THORN . 222)
    (infin . 8734) (yuml . 255) (Mu . 924) (le . 8804) (Eacute . 201)
    (thinsp . 8201) (ecirc . 234) (bdquo . 8222) (Sigma . 931) (fnof . 402)
    (kappa . 954) (Aring . 197) (tilde . 732) (cup . 8746) (mdash . 8212)
    (uarr . 8593) (permil . 8240) (tau . 964) (Ugrave . 217) (eta . 951)
    (Agrave . 192) (sup1 . 185) (forall . 8704) (eth . 240) (rceil . 8969)
    (iuml . 239) (gamma . 947) (lambda . 955) (harr . 8596) (reg . 174)
    (Egrave . 200) (sup3 . 179) (dagger . 8224) (divide . 247) (Ouml . 214)
    (image . 8465) (alefsym . 8501) (igrave . 236) (otilde . 245)
    (pound . 163) (eacute . 233) (frasl . 8260) (ETH . 208) (lowast . 8727)
    (Nu . 925) (plusmn . 177) (chi . 967) (sup2 . 178) (frac34 . 190)
    (Aacute . 193) (cent . 162) (oline . 8254) (Beta . 914) (perp . 8869)
    (Delta . 916) (loz . 9674) (pi . 960) (iota . 953) (empty . 8709)
    (euml . 235) (brvbar . 166) (iacute . 237) (para . 182) (ordm . 186)
    (ensp . 8194) (uuml . 252) (there4 . 8756) (part . 8706) (icirc . 238)
    (bull . 8226) (omicron . 959) (upsilon . 965) (copy . 169) (Iuml . 207)
    (Oacute . 211) (Xi . 926) (Dagger . 8225) (Ograve . 210) (Ucirc . 219)
    (cap . 8745) (mu . 956) (sigmaf . 962) (scaron . 353) (lsquo . 8216)
    (isin . 8712) (Zeta . 918) (minus . 8722) (deg . 176) (and . 8743)
    (real . 8476) (ang . 8736) (hellip . 8230) (curren . 164) (int . 8747)
    (ucirc . 251) (rfloor . 8971) (crarr . 8629) (ugrave . 249)
    (notin . 8713) (exist . 8707) (cong . 8773) (oplus . 8853) (times . 215)
    (Acirc . 194) (piv . 982) (Euml . 203) (Phi . 934) (Iacute . 205)
    (quot . 34) (Uacute . 218) (Omicron . 927) (ne . 8800) (Iota . 921)
    (nabla . 8711) (sbquo . 8218) (Rho . 929) (epsilon . 949) (Ecirc . 202)
    (zeta . 950) (Omega . 937) (acirc . 226) (sim . 8764) (phi . 966)
    (diams . 9830) (macr . 175) (larr . 8592) (Ccedil . 199) (aacute . 225)
    (uArr . 8657) (beta . 946) (Eta . 919) (weierp . 8472) (rho . 961)
    (micro . 181) (alpha . 945) (omega . 969) (middot . 183) (Gamma . 915)
    (euro . 8364) (lang . 9001) (spades . 9824) (rsquo . 8217) (uml . 168)
    (thorn . 254) (ouml . 246) (thetasym . 977) (or . 8744) (raquo . 187)
    (sect . 167) (ldquo . 8220) (hearts . 9829) (sigma . 963) (oacute . 243))
  )

(define html-entity-unicode-chars 
  (map 
    (lambda (e) (cons (car e) (make-string 1 (integer->char (cdr e))))) 
    html-entity-unicode-numbers))

