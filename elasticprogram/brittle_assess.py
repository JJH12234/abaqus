# -*- coding: utf-8 -*-
import numpy as np
import math
import Tkinter as tk
import tkFileDialog as filedialog
import tkMessageBox as messagebox
import csv
import os, sys, subprocess
import traceback
import multiprocessing as mp
import os, sys, subprocess, platform
import threading
import time
def launch_brittle_gui():
    print(55555)
    python_exe  = sys.executable            # 若 Abaqus-Python 缺 Tk，就换系统 python
    print(666666)
    plugin_dir  = os.path.dirname(__file__)
    print(7777777)

    inline = (
        "import sys, os;"
        "sys.path.insert(0, r'{0}');"
        "import brittle_assess as _m;"
        "_m.run_gui()"
    ).format(plugin_dir.replace('\\', '\\\\'))

    flags = subprocess.CREATE_NEW_CONSOLE if platform.system()=="Windows" else 0
    subprocess.Popen([python_exe, "-u", "-c", inline],
                     creationflags=flags)
def launch_brittle_gui_delayed():
    """
    延迟启动GUI，让ABAQUS主循环有时间处理完成
    """
    def delayed_start():
        print("等待2秒后启动GUI...")
        time.sleep(2)  # 等待2秒
        print("开始启动GUI...")
        run_gui()
    
    # 在新线程中延迟启动
    gui_thread = threading.Thread(target=delayed_start)
    gui_thread.daemon = False  # 非守护线程
    gui_thread.start()
    print("GUI将在2秒后启动...")

# 或者更简单的方案，直接在主调用中延迟
def launch_brittle_gui_simple():
    """
    最简单有效的解决方案
    """
    import threading
    import time
    
    def gui_worker():
        time.sleep(10)  # 让主程序先继续
        try:
            run_gui()
        except Exception as e:
            print("GUI启动失败: {}".format(e))
    
    thread = threading.Thread(target=gui_worker)
    thread.daemon = True  # 重要：守护线程
    thread.start()
    print("GUI启动中...")


# # 读取文本文件
def read_data(file_path):
    """ 读取 txt 文件中的数据 """
    data = np.genfromtxt(file_path)
    # 数据清洗 - 移除包含NaN的行
    return data[~np.isnan(data).any(axis=1)]

# 二次多项式拟�?
def quadratic_poly_fit(x, y):
    """ 对数据进行二次多项式拟合 """
    coefficients = np.polyfit(x, y, 2)
    return coefficients

# 多项式积�?
def integrate_polynomial(coefficients):
    """ 对二次多项式进行积分，返回积分后的三次多项式系数 """
    return [
        coefficients[0] / 3,  # a2 -> a3
        coefficients[1] / 2,  # a1 -> a2
        coefficients[2],      # a0 -> a1
        0                     # 积分常数项（默认 0�?
    ]

# 计算薄膜应力 sigma_p
def calculate_sigma_p(integrated_coeffs, t):
    """ 计算薄膜应力 sigma_p """
    return np.polyval(integrated_coeffs, t) / t

# 计算应力强度因子 KI
def calculate_KI(sigma_p, sigma_q, t):
    """ 计算应力强度因子 KI """
    return (sigma_p * 1.08 + sigma_q * 0.68) * (math.pi * 0.25 * t / 1000) ** 0.5 * 1.11

# 计算温度 T
def calculate_temperature(coefficients, t):
    """ 计算 0.25t 处的温度 """
    return np.polyval(coefficients, 0.25 * t)

# 计算断裂韧�? KIc（按公式计算�?
def calculate_KIc(T, Tk):
    """ 计算断裂韧�? KIc，最大�? 220 """
    return min(26 + 36 * math.exp(0.02 * (T - Tk)), 220)

# 评估是否通过
def assess_KI_vs_KIc(KI, KIc, k):
    """ 评估 KI 是否小于 KIc/k """
    return u"通过" if KI < KIc / k else u"不通过"

# 主程�?
def main(file_path_stress, file_path_temperature, t, k, Tk=None, KIc_method=0, fixed_KIc=None):
    """
    计算应力、温度、断裂韧�? KIc，并进行合格评估�?

    参数�?
    - file_path_stress: 应力数据文件
    - file_path_temperature: 温度数据文件
    - t: 壁厚
    - k: 安全系数
    - Tk: 参考温度（仅在 KIc_method=0 时使用）
    - KIc_method: 计算方式�?0=按公式计算，1=使用固定值）
    - fixed_KIc: 固定�? KIc 值（仅在 KIc_method=1 时使用）
    """
    # 读取数据
    data_stress = read_data(file_path_stress)
    data_temperature = read_data(file_path_temperature)

    # 定义 x 轴（�? 0 �? t 均匀分布�?
    num_points = data_stress.shape[1] - 1  # 除去时间�?
    x = np.linspace(0, t, num_points)

    # 结果存储
    time_results = data_stress[:, 0]  # 获取时间�?
    sigma_p_results = []
    sigma_q_results = []
    KI_results = []
    temperature_results = []
    KIc_results = []
    assessment_results = []

    # 逐行计算
    for i in range(data_stress.shape[0]):
        y_stress = data_stress[i, 1:]  # 获取应力数据
        y_temperature = data_temperature[i, 1:]  # 获取温度数据

        # 拟合应力数据
        coefficients_stress = quadratic_poly_fit(x, y_stress)
        integrated_coeffs = integrate_polynomial(coefficients_stress)

        # 计算薄膜应力 sigma_p
        sigma_p = calculate_sigma_p(integrated_coeffs, t)
        sigma_p_results.append(sigma_p)

        # 计算弯曲应力 sigma_q
        sigma_q = data_stress[i, 1] - sigma_p  # �?2列数�? - sigma_p
        sigma_q_results.append(sigma_q)

        # 计算应力强度因子 KI
        KI = calculate_KI(sigma_p, sigma_q, t)
        KI_results.append(KI)

        # 拟合温度数据并计�? 0.25t 处的温度 T
        coefficients_temperature = quadratic_poly_fit(x, y_temperature)
        T = calculate_temperature(coefficients_temperature, t)
        temperature_results.append(T)

        # 计算 KIc
        if KIc_method == 0:
            KIc = calculate_KIc(T, Tk)  # 按公式计�?
        elif KIc_method == 1:
            KIc = fixed_KIc  # 采用固定�?
        else:
            raise ValueError("KIc_method 只能�? 0（公式计算）�? 1（固定值）")
        KIc_results.append(KIc)

        # 评估是否通过
        assessment = assess_KI_vs_KIc(KI, KIc, k)
        assessment_results.append(assessment)

    # 将结果保存为CSV
    output_file = '脆断评定结果.csv'
    with open(output_file, 'wb') as f:
        writer = csv.writer(f)
        # 写入表头
        writer.writerow(['时间', '薄膜应力', '弯曲应力', '应力强度因子', '温度', '断裂韧�?', '评估结果'])
        # 写入数据
        for i in range(len(time_results)):
            writer.writerow([
                time_results[i],
                sigma_p_results[i],
                sigma_q_results[i],
                KI_results[i],
                temperature_results[i],
                KIc_results[i],
                assessment_results[i].encode('utf-8')  # 处理中文字符
            ])
    
    messagebox.showinfo("完成", "计算结果已保存到 {}".format(output_file))

# GUI界面
def run_gui():
    def browse_file(file_type):
        filename = filedialog.askopenfilename(filetypes=[("{} 文件".format(file_type), "*.txt")])
        if file_type == "应力":
            entry_file_stress.delete(0, tk.END)
            entry_file_stress.insert(0, filename)
        else:
            entry_file_temperature.delete(0, tk.END)
            entry_file_temperature.insert(0, filename)

    def start_calculation():
        try:
            file_path_stress = entry_file_stress.get()
            file_path_temperature = entry_file_temperature.get()
            t = float(entry_thickness.get())
            k = float(entry_safety_factor.get())
            Tk = float(entry_reference_temp.get())
            KIc_method = int(var_KIc_method.get())
            fixed_KIc = float(entry_fixed_KIc.get()) if KIc_method == 1 else None

            main(file_path_stress, file_path_temperature, t, k, Tk, KIc_method, fixed_KIc)
        except Exception as e:
            messagebox.showerror("错误", "发生错误: {}".format(e))

    # 创建主窗�?
    root = tk.Tk()
    root.title("防脆断评定工�?")
    root.geometry("850x450")  # 增大窗口尺寸

    # 设置背景颜色
    root.configure(bg="#f0f0f0")

    # 设置字体
    font_large = ("Microsoft YaHei", 12)
    font_medium = ("Microsoft YaHei", 10)

    # 设置UI元素
    tk.Label(root, text="应力数据文件:", bg="#f0f0f0", font=font_large).grid(row=0, column=0, padx=10, pady=10, sticky="w")
    entry_file_stress = tk.Entry(root, width=70, font=font_medium)
    entry_file_stress.grid(row=0, column=1, padx=10, pady=10)
    tk.Button(root, text="浏览", command=lambda: browse_file("应力"), bg="teal", fg="white", font=font_medium).grid(row=0, column=2, padx=10, pady=10)

    tk.Label(root, text="温度数据文件:", bg="#f0f0f0", font=font_large).grid(row=1, column=0, padx=10, pady=10, sticky="w")
    entry_file_temperature = tk.Entry(root, width=70, font=font_medium)
    entry_file_temperature.grid(row=1, column=1, padx=10, pady=10)
    tk.Button(root, text="浏览", command=lambda: browse_file("温度"), bg="teal", fg="white", font=font_medium).grid(row=1, column=2, padx=10, pady=10)

    tk.Label(root, text="壁厚 (t):", bg="#f0f0f0", font=font_large).grid(row=2, column=0, padx=10, pady=10, sticky="w")
    entry_thickness = tk.Entry(root, font=font_medium)
    entry_thickness.grid(row=2, column=1, padx=10, pady=10)

    tk.Label(root, text="安全系数 (k):", bg="#f0f0f0", font=font_large).grid(row=3, column=0, padx=10, pady=10, sticky="w")
    entry_safety_factor = tk.Entry(root, font=font_medium)
    entry_safety_factor.grid(row=3, column=1, padx=10, pady=10)

    tk.Label(root, text="参考温�? (Tk):", bg="#f0f0f0", font=font_large).grid(row=4, column=0, padx=10, pady=10, sticky="w")
    entry_reference_temp = tk.Entry(root, font=font_medium)
    entry_reference_temp.grid(row=4, column=1, padx=10, pady=10)

    tk.Label(root, text="KIc计算方式:", bg="#f0f0f0", font=font_large).grid(row=5, column=0, padx=10, pady=10, sticky="w")
    var_KIc_method = tk.StringVar(value="0")
    tk.Radiobutton(root, text="按公式计�?", variable=var_KIc_method, value="0", font=font_medium).grid(row=5, column=1, padx=10, pady=10, sticky="w")
    tk.Radiobutton(root, text="使用固定�?", variable=var_KIc_method, value="1", font=font_medium).grid(row=5, column=2, padx=10, pady=10, sticky="w")

    tk.Label(root, text="固定KIc�?:", bg="#f0f0f0", font=font_large).grid(row=6, column=0, padx=10, pady=10, sticky="w")
    entry_fixed_KIc = tk.Entry(root, font=font_medium)
    entry_fixed_KIc.grid(row=6, column=1, padx=10, pady=10)

    tk.Button(root, text="开始计�?", command=start_calculation, bg="teal", fg="white", font=("Microsoft YaHei", 14), width=20).grid(row=7, column=0, columnspan=3, pady=20)

    root.mainloop()

# 启动UI
if __name__=="__main__":
    from multiprocessing import freeze_support  # 安全起见
    freeze_support()
    run_gui()       # ← 关键调用