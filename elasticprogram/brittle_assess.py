# -*- coding: utf-8 -*-
"""
脆断评定工具
Python 2.7 + Tkinter（兼容 Abaqus 自带解释器）
"""

from __future__ import unicode_literals
import numpy as np
import math
import Tkinter as tk
import tkFileDialog as filedialog
import tkMessageBox as messagebox
import csv
from multiprocessing import freeze_support


# ──────────────────────────────── 通用函数 ────────────────────────────────
def read_data(file_path):
    """读取 txt 中的数值并剔除含 NaN 行"""
    data = np.genfromtxt(file_path)
    return data[~np.isnan(data).any(axis=1)]


def quadratic_poly_fit(x, y):
    """二次多项式拟合"""
    return np.polyfit(x, y, 2)


def integrate_polynomial(coeffs):
    """二次多项式积分，得到三次多项式系数"""
    return [coeffs[0] / 3.0, coeffs[1] / 2.0, coeffs[2], 0.0]


def calculate_sigma_p(integ_coeffs, t):
    """薄膜应力 σ_p"""
    return np.polyval(integ_coeffs, t) / t


def calculate_KI(sigma_p, sigma_q, t):
    """应力强度因子 KI"""
    return (sigma_p * 1.08 + sigma_q * 0.68) * (math.pi * 0.25 * t / 1000.0) ** 0.5 * 1.11


def calculate_temperature(coeffs, t):
    """0.25t 处温度"""
    return np.polyval(coeffs, 0.25 * t)


def calculate_KIc(T, Tk):
    """断裂韧度 KIc（公式法，最大 220 MPa·m^0.5）"""
    return min(26 + 36 * math.exp(0.02 * (T - Tk)), 220)


def assess_KI_vs_KIc(KI, KIc, k):
    """评定结论"""
    return u"通过" if KI < KIc / k else u"不通过"


# ──────────────────────────────── 计算主流程 ────────────────────────────────
def main(file_stress, file_temp, t, k,
         Tk=None, KIc_method=0, fixed_KIc=None):
    # 1. 读取数据
    data_stress = read_data(file_stress)
    data_temp   = read_data(file_temp)

    num_pts = data_stress.shape[1] - 1
    x = np.linspace(0, t, num_pts)

    # 2. 初始化结果容器
    time_col = data_stress[:, 0]
    sigma_p_list, sigma_q_list = [], []
    KI_list, temp_list, KIc_list, result_list = [], [], [], []

    # 3. 循环处理每一行
    for i in range(data_stress.shape[0]):
        y_stress = data_stress[i, 1:]
        y_temp   = data_temp[i,   1:]

        # ─ 应力 ─
        coeffs_stress = quadratic_poly_fit(x, y_stress)
        coeffs_int    = integrate_polynomial(coeffs_stress)
        sigma_p = calculate_sigma_p(coeffs_int, t)
        sigma_q = data_stress[i, 1] - sigma_p

        KI = calculate_KI(sigma_p, sigma_q, t)

        # ─ 温度 ─
        coeffs_temp = quadratic_poly_fit(x, y_temp)
        T = calculate_temperature(coeffs_temp, t)

        # ─ KIc ─
        if KIc_method == 0:
            KIc = calculate_KIc(T, Tk)
        else:
            KIc = fixed_KIc

        verdict = assess_KI_vs_KIc(KI, KIc, k)

        # 收集
        sigma_p_list.append(sigma_p)
        sigma_q_list.append(sigma_q)
        KI_list.append(KI)
        temp_list.append(T)
        KIc_list.append(KIc)
        result_list.append(verdict)

    # 4. 保存 CSV（GBK 编码）
    out_csv = u"脆断评定结果.csv"
    with open(out_csv, "wb") as f:
        writer = csv.writer(f)

        headers = [u"时间", u"薄膜应力", u"弯曲应力",
                   u"应力强度因子", u"温度", u"断裂韧度",
                   u"评估结果"]
        writer.writerow([h.encode("gbk") for h in headers])

        for i in range(len(time_col)):
            writer.writerow([
                time_col[i],
                sigma_p_list[i],
                sigma_q_list[i],
                KI_list[i],
                temp_list[i],
                KIc_list[i],
                result_list[i].encode("gbk")
            ])

    messagebox.showinfo(u"完成", u"计算结果已保存到 {}".format(out_csv))


# ──────────────────────────────── 图形界面 ────────────────────────────────
def run_gui():
    # ─ 内部工具函数 ─
    def browse(file_tag):
        path = filedialog.askopenfilename(filetypes=[(u"{} 文件".format(file_tag), "*.txt")])
        if not path:
            return
        if file_tag == u"应力":
            ent_stress.delete(0, tk.END)
            ent_stress.insert(0, path)
        else:
            ent_temp.delete(0, tk.END)
            ent_temp.insert(0, path)

    def start():
        try:
            f_stress = ent_stress.get()
            f_temp   = ent_temp.get()
            t_val = float(ent_t.get())
            k_val = float(ent_k.get())

            method  = int(var_method.get())
            Tk_val  = float(ent_Tk.get())  if method == 0 else None
            KIc_val = float(ent_KIc.get()) if method == 1 else None

            main(f_stress, f_temp, t_val, k_val,
                 Tk=Tk_val, KIc_method=method, fixed_KIc=KIc_val)
        except Exception as e:
            messagebox.showerror(u"错误", u"发生错误：{}".format(unicode(e)))

    # ─ 创建窗口 ─
    root = tk.Tk()
    root.title(u"防脆断评定工具")
    root.geometry("850x450")
    root.configure(bg="#f0f0f0")

    f_large = ("Microsoft YaHei", 12)
    f_mid   = ("Microsoft YaHei", 10)

    # 行 0 : 应力文件
    tk.Label(root, text=u"应力数据文件:", bg="#f0f0f0", font=f_large
             ).grid(row=0, column=0, padx=10, pady=10, sticky="w")
    ent_stress = tk.Entry(root, width=70, font=f_mid)
    ent_stress.grid(row=0, column=1, padx=10, pady=10)
    tk.Button(root, text=u"浏览", command=lambda: browse(u"应力"),
              bg="teal", fg="white", font=f_mid
              ).grid(row=0, column=2, padx=10, pady=10)

    # 行 1 : 温度文件
    tk.Label(root, text=u"温度数据文件:", bg="#f0f0f0", font=f_large
             ).grid(row=1, column=0, padx=10, pady=10, sticky="w")
    ent_temp = tk.Entry(root, width=70, font=f_mid)
    ent_temp.grid(row=1, column=1, padx=10, pady=10)
    tk.Button(root, text=u"浏览", command=lambda: browse(u"温度"),
              bg="teal", fg="white", font=f_mid
              ).grid(row=1, column=2, padx=10, pady=10)

    # 行 2 : 壁厚 t
    tk.Label(root, text=u"壁厚 (t):", bg="#f0f0f0", font=f_large
             ).grid(row=2, column=0, padx=10, pady=10, sticky="w")
    ent_t = tk.Entry(root, font=f_mid)
    ent_t.grid(row=2, column=1, padx=10, pady=10, sticky="w")

    # 行 3 : 安全系数 k
    tk.Label(root, text=u"安全系数 (k):", bg="#f0f0f0", font=f_large
             ).grid(row=3, column=0, padx=10, pady=10, sticky="w")
    ent_k = tk.Entry(root, font=f_mid)
    ent_k.grid(row=3, column=1, padx=10, pady=10, sticky="w")

    # 行 4 : 参考温度 Tk
    tk.Label(root, text=u"参考温度 (Tk):", bg="#f0f0f0", font=f_large
             ).grid(row=4, column=0, padx=10, pady=10, sticky="w")
    ent_Tk = tk.Entry(root, font=f_mid)
    ent_Tk.grid(row=4, column=1, padx=10, pady=10, sticky="w")

    # 行 5 : KIc 计算方式
    tk.Label(root, text=u"KIc 计算方式:", bg="#f0f0f0", font=f_large
             ).grid(row=5, column=0, padx=10, pady=10, sticky="w")

    frame_method = tk.Frame(root, bg="#f0f0f0")
    frame_method.grid(row=5, column=1, columnspan=2, padx=10, pady=10, sticky="w")

    var_method = tk.StringVar(value="0")
    tk.Radiobutton(frame_method, text=u"按公式计算",
                   variable=var_method, value="0",
                   font=f_mid, bg="#f0f0f0"
                   ).pack(side="left", padx=5)
    tk.Radiobutton(frame_method, text=u"使用固定值",
                   variable=var_method, value="1",
                   font=f_mid, bg="#f0f0f0"
                   ).pack(side="left", padx=5)

    # 行 6 : 固定 KIc 值
    tk.Label(root, text=u"固定 KIc 值:", bg="#f0f0f0", font=f_large
             ).grid(row=6, column=0, padx=10, pady=10, sticky="w")
    ent_KIc = tk.Entry(root, font=f_mid)
    ent_KIc.grid(row=6, column=1, padx=10, pady=10, sticky="w")

    # 行 7 : 开始按钮
    tk.Button(root, text=u"开始计算", command=start,
              bg="teal", fg="white",
              font=("Microsoft YaHei", 14), width=20
              ).grid(row=7, column=0, columnspan=3, pady=25)

    root.mainloop()


# ──────────────────────────────── 独立启动 ────────────────────────────────
if __name__ == "__main__":
    freeze_support()   # Windows 下多进程安全
    run_gui()
