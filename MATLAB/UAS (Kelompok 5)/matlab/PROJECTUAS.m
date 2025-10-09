function varargout = PROJECTUAS(varargin)
% PROJECTUAS MATLAB code for PROJECTUAS.fig
% ... (bagian awal kode biarkan seperti aslinya) ...

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @PROJECTUAS_OpeningFcn, ...
                   'gui_OutputFcn',  @PROJECTUAS_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end
if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT

% --- Executes just before PROJECTUAS is made visible.
function PROJECTUAS_OpeningFcn(hObject, eventdata, handles, varargin)
handles.output = hObject;
guidata(hObject, handles);

% --- Outputs from this function are returned to the command line.
function varargout = PROJECTUAS_OutputFcn(hObject, eventdata, handles) 
varargout{1} = handles.output;

% --- Executes on button press in pushbutton9. (Hapus)
function pushbutton9_Callback(hObject, eventdata, handles)
set(handles.edit1,'string',''); 
set(handles.edit2,'string',''); 
cla(handles.axes1, 'reset'); 
legend(handles.axes1, 'off'); 
title(handles.axes1, ''); 

% --- Executes on button press in pushbutton10. (Keluar)
function pushbutton10_Callback(hObject, eventdata, handles)
p = questdlg('Yakin Anda Keluar?', 'Konfirmasi Keluar', 'Ya', 'Tidak', 'Ya'); 
switch p 
case 'Ya' 
    delete(handles.figure1);
end

% --- Executes on button press in pushbutton1. (f(x) + g(x))
function pushbutton1_Callback(hObject, eventdata, handles)
try
    syms x 
    f_str = get(handles.edit1,'String'); 
    g_str = get(handles.edit2,'String'); 
    
    if isempty(f_str) || isempty(g_str)
        warndlg('Harap masukkan kedua fungsi f(x) dan g(x).', 'Input Tidak Lengkap');
        return;
    end
    
    f_sym = str2sym(f_str);
    g_sym = str2sym(g_str);
    h_sym = f_sym + g_sym;
    
    cla(handles.axes1, 'reset'); 
    fplot(handles.axes1, h_sym); 
    grid(handles.axes1, 'on'); 
    legend(handles.axes1, ['Hasil: ', char(h_sym)]); % MENAMPILKAN HASIL DI LEGEND
    title(handles.axes1, 'Penjumlahan Fungsi: f(x) + g(x)'); 
catch ME
    warndlg(['Terjadi kesalahan: ', ME.message], 'Error Kalkulasi');
    cla(handles.axes1, 'reset'); 
    legend(handles.axes1, 'off');
    title(handles.axes1, '');
end

% --- Executes on button press in pushbutton2. (f(x) * g(x))
function pushbutton2_Callback(hObject, eventdata, handles)
try
    syms x 
    f_str = get(handles.edit1,'String'); 
    g_str = get(handles.edit2,'String'); 
    
    if isempty(f_str) || isempty(g_str)
        warndlg('Harap masukkan kedua fungsi f(x) dan g(x).', 'Input Tidak Lengkap');
        return;
    end
    
    f_sym = str2sym(f_str);
    g_sym = str2sym(g_str);
    
    h_sym = f_sym * g_sym; 
    i_result = expand(h_sym); 
    
    cla(handles.axes1, 'reset'); 
    fplot(handles.axes1, i_result); 
    grid(handles.axes1, 'on'); 
    legend(handles.axes1, ['Hasil: ', char(i_result)]); % MENAMPILKAN HASIL DI LEGEND
    title(handles.axes1, 'Perkalian Fungsi: f(x) * g(x)');
catch ME
    warndlg(['Terjadi kesalahan: ', ME.message], 'Error Kalkulasi');
    cla(handles.axes1, 'reset');
    legend(handles.axes1, 'off');
    title(handles.axes1, '');
end

% --- Executes on button press in pushbutton3. (f(x) / g(x))
function pushbutton3_Callback(hObject, eventdata, handles)
try
    syms x 
    f_str = get(handles.edit1,'String'); 
    g_str = get(handles.edit2,'String'); 
    
    if isempty(f_str) || isempty(g_str)
        warndlg('Harap masukkan kedua fungsi f(x) dan g(x).', 'Input Tidak Lengkap');
        return;
    end
    
    f_sym = str2sym(f_str);
    g_sym = str2sym(g_str);
    
    if isequal(g_sym, 0) 
        warndlg('Fungsi g(x) tidak boleh nol untuk pembagian.', 'Error Pembagian');
        cla(handles.axes1, 'reset'); legend(handles.axes1, 'off'); title(handles.axes1, '');
        return;
    end
    
    h_sym = f_sym / g_sym; 
    i_result = expand(h_sym); 
    
    cla(handles.axes1, 'reset'); 
    fplot(handles.axes1, i_result); 
    grid(handles.axes1, 'on'); 
    legend(handles.axes1, ['Hasil: ', char(i_result)]); % MENAMPILKAN HASIL DI LEGEND
    title(handles.axes1, 'Pembagian Fungsi: f(x) / g(x)');
catch ME
    warndlg(['Terjadi kesalahan: ', ME.message], 'Error Kalkulasi');
    cla(handles.axes1, 'reset');
    legend(handles.axes1, 'off');
    title(handles.axes1, '');
end

% --- Executes on button press in pushbutton4. (f(g(x)))
function pushbutton4_Callback(hObject, eventdata, handles)
try
    syms x
    f_str = get(handles.edit1,'String');
    g_str = get(handles.edit2,'String');
    
    if isempty(f_str) || isempty(g_str)
        warndlg('Harap masukkan kedua fungsi f(x) dan g(x).', 'Input Tidak Lengkap');
        return;
    end
    
    f_sym = str2sym(f_str);
    g_sym = str2sym(g_str);
    
    h_compose = subs(f_sym, x, g_sym); 
    i_result = expand(h_compose); 
    
    cla(handles.axes1, 'reset'); 
    fplot(handles.axes1, i_result);
    grid(handles.axes1, 'on');
    legend(handles.axes1, ['Hasil: ', char(i_result)]); % MENAMPILKAN HASIL DI LEGEND
    title(handles.axes1, 'Komposisi Fungsi: f(g(x))');
catch ME
    warndlg(['Terjadi kesalahan: ', ME.message], 'Error Kalkulasi');
    cla(handles.axes1, 'reset');
    legend(handles.axes1, 'off');
    title(handles.axes1, '');
end

% --- Executes on button press in pushbutton5. (f(x) - g(x))
function pushbutton5_Callback(hObject, eventdata, handles)
try
    syms x 
    f_str = get(handles.edit1,'String'); 
    g_str = get(handles.edit2,'String'); 
    
    if isempty(f_str) || isempty(g_str)
        warndlg('Harap masukkan kedua fungsi f(x) dan g(x).', 'Input Tidak Lengkap');
        return;
    end
    
    f_sym = str2sym(f_str);
    g_sym = str2sym(g_str);
    
    h_sym = f_sym - g_sym; 
    
    cla(handles.axes1, 'reset'); 
    fplot(handles.axes1, h_sym); 
    grid(handles.axes1, 'on'); 
    legend(handles.axes1, ['Hasil: ', char(h_sym)]); % MENAMPILKAN HASIL DI LEGEND
    title(handles.axes1, 'Pengurangan Fungsi: f(x) - g(x)');
catch ME
    warndlg(['Terjadi kesalahan: ', ME.message], 'Error Kalkulasi');
    cla(handles.axes1, 'reset');
    legend(handles.axes1, 'off');
    title(handles.axes1, '');
end

% --- Executes on button press in pushbutton6. (g(x) - f(x))
function pushbutton6_Callback(hObject, eventdata, handles)
try
    syms x 
    f_str = get(handles.edit1,'String'); 
    g_str = get(handles.edit2,'String'); 
    
    if isempty(f_str) || isempty(g_str)
        warndlg('Harap masukkan kedua fungsi f(x) dan g(x).', 'Input Tidak Lengkap');
        return;
    end
    
    f_sym = str2sym(f_str);
    g_sym = str2sym(g_str);
    
    h_sym = g_sym - f_sym; 
    
    cla(handles.axes1, 'reset'); 
    fplot(handles.axes1, h_sym); 
    grid(handles.axes1, 'on'); 
    legend(handles.axes1, ['Hasil: ', char(h_sym)]); % MENAMPILKAN HASIL DI LEGEND
    title(handles.axes1, 'Pengurangan Fungsi: g(x) - f(x)'); 
catch ME
    warndlg(['Terjadi kesalahan: ', ME.message], 'Error Kalkulasi');
    cla(handles.axes1, 'reset');
    legend(handles.axes1, 'off');
    title(handles.axes1, '');
end

% --- Executes on button press in pushbutton7. (g(x) / f(x))
function pushbutton7_Callback(hObject, eventdata, handles)
try
    syms x 
    f_str = get(handles.edit1,'String'); 
    g_str = get(handles.edit2,'String'); 
    
    if isempty(f_str) || isempty(g_str)
        warndlg('Harap masukkan kedua fungsi f(x) dan g(x).', 'Input Tidak Lengkap');
        return;
    end
    
    f_sym = str2sym(f_str);
    g_sym = str2sym(g_str);
    
    if isequal(f_sym, 0) 
        warndlg('Fungsi f(x) tidak boleh nol untuk pembagian.', 'Error Pembagian');
        cla(handles.axes1, 'reset'); legend(handles.axes1, 'off'); title(handles.axes1, '');
        return;
    end
    
    h_sym = g_sym / f_sym; 
    i_result = expand(h_sym); 
    
    cla(handles.axes1, 'reset'); 
    fplot(handles.axes1, i_result); 
    grid(handles.axes1, 'on'); 
    legend(handles.axes1, ['Hasil: ', char(i_result)]); % MENAMPILKAN HASIL DI LEGEND
    title(handles.axes1, 'Pembagian Fungsi: g(x) / f(x)');
catch ME
    warndlg(['Terjadi kesalahan: ', ME.message], 'Error Kalkulasi');
    cla(handles.axes1, 'reset');
    legend(handles.axes1, 'off');
    title(handles.axes1, '');
end

% --- Executes on button press in pushbutton8. (g(f(x)))
function pushbutton8_Callback(hObject, eventdata, handles)
try
    syms x 
    f_str = get(handles.edit1,'String'); 
    g_str = get(handles.edit2,'String'); 
    
    if isempty(f_str) || isempty(g_str)
        warndlg('Harap masukkan kedua fungsi f(x) dan g(x).', 'Input Tidak Lengkap');
        return;
    end
    
    f_sym = str2sym(f_str);
    g_sym = str2sym(g_str);
    
    h_compose = subs(g_sym, x, f_sym); 
    i_result = expand(h_compose); 
    
    cla(handles.axes1, 'reset'); 
    fplot(handles.axes1, i_result); 
    grid(handles.axes1, 'on'); 
    legend(handles.axes1, ['Hasil: ', char(i_result)]); % MENAMPILKAN HASIL DI LEGEND
    title(handles.axes1, 'Komposisi Fungsi: g(f(x))'); 
catch ME
    warndlg(['Terjadi kesalahan: ', ME.message], 'Error Kalkulasi');
    cla(handles.axes1, 'reset');
    legend(handles.axes1, 'off');
    title(handles.axes1, '');
end

% --- Callback untuk edit1 dan edit2 
function edit1_Callback(hObject, eventdata, handles)
% --- Executes during object creation, after setting all properties.
function edit1_CreateFcn(hObject, eventdata, handles)
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

function edit2_Callback(hObject, eventdata, handles)
% --- Executes during object creation, after setting all properties.
function edit2_CreateFcn(hObject, eventdata, handles)
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
