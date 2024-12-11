
%Annahme: Subscription kann z.B. alle 30s wiederholt werden. 
%
%Subscription handling
%mit parameter 'ich habe die konten bis kontonumer X' + Subscriber PID
% 1. Fall X kleiner current:
% Antwort: delta(x bis current).
% 2. Fall X >= current:
% Antwort: keine.

% Aliveness-Problematik 
% Bei 1 u. 2
% Merken von Subscriber PID für in Liste
% A. Wenn PID bereits bekannt, keine aufnahme ins Monitoring aber antworten wie bei 1. 
% B. Wenn PID nicht bekannt, aufnahme ins Monitoring und antworten wie bei 1. 
% Bei Monitor-Fail: entfernen aus PID Liste

% Mitbekommen von Account-Neuanlagen
% Bei Accountneuanlage: Antwort an alle in PID-Liste mit neuem Account.