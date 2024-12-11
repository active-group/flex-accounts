
%Annahme: Subscription kann z.B. alle 30s wiederholt werden. 
%
% Subscription handling
%   -> mit parameter 'ich habe die Accounts bis Accountnumber X' + Subscriber PID
% 1. Fall X kleiner Current-Accountnumber:
% Antwort: delta(X bis Current-Accountnumber).
% 2. Fall X >= Current-Accountnumber:
% Antwort: keine. Weil Asyncron
% Struktur:  
% Antwort-Format: {}

% Aliveness-Problematik 
% Bei 1 u. 2
% Merken von Subscriber-PID in Liste.
% A. Wenn PID bereits bekannt, keine aufnahme ins Monitoring aber antworten wie bei 1. 
% B. Wenn PID nicht bekannt, aufnahme ins Monitoring und antworten wie bei 1. 
% Bei Monitor-Fail: entfernen aus PID-Liste.

% Mitbekommen von Account-Neuanlagen
% Bei Accountneuanlage: Antwort an alle in PID-Liste mit neuem Account.