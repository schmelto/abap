vfx3 -> da bleibt alles hängen was in der Buchhaltung hängenbleibt

vkoa -> Kontenfindung -> 0001





![image](https://user-images.githubusercontent.com/30869493/117456323-5c8a3680-af48-11eb-94d0-8012273303ce.png)





folgendermaßen debugge ich einen Zahlungsvorschlag:
1) Breakpoint setzen z.B. im Funktionsbaustein
2) F110 Vorschlag einplanen ohne Sofortstart, sondern mit Startzeit z.b. 23:00:00 -> Somit wird ein Job erzeugt
3) Jobübersicht SM37/SMX -> Job markieren -> im OK-code "JDBG" eingeben (somit wird der Job gestartet!)
