# -*- coding: utf-8 -*-
# 这是一个指定文件编码为 UTF-8 的特殊注释，确保中文能正确显示。
"""
脆断评定工具（可输入 η 系数）
Python 2.7 + Tkinter（兼容 Abaqus 自带解释器）
"""
# 上述多行字符串是模块的文档字符串，描述了工具的用途、兼容性及主要特性。
# 它说明这是一个用于脆断评定、支持输入 eta (η) 系数的工具。
# 指明了开发环境是 Python 2.7 和 Tkinter。
# 特别强调了兼容 Abaqus 软件自带的 Python 解释器，这对于在特定工程软件环境中运行很重要。

from __future__ import unicode_literals
# 导入 future 模块的 unicode_literals，使得所有字符串字面量默认为 Unicode 类型。
# 这有助于在 Python 2 中处理中文，避免编码问题，提高字符串处理的兼容性。
import numpy as np
# 导入 NumPy 库，用于进行数值计算，特别是数组操作和多项式拟合。
# 将其别名为 np，方便后续代码调用，是科学计算的核心库。
import math
# 导入 math 模块，提供数学函数，如平方根 (sqrt) 和圆周率 (pi)。
# 用于执行基本的数学运算。
import Tkinter as tk
# 导入 Tkinter 库，这是 Python 的标准 GUI (图形用户界面) 库。
# 将其别名为 tk，用于创建窗口、按钮、输入框等界面元素，构建用户友好的交互界面。
import tkFileDialog as filedialog
# 导入 tkFileDialog 模块，用于提供文件选择对话框功能。
# 允许用户浏览并选择本地文件，例如应力数据和温度数据文件。
import tkMessageBox as messagebox
# 导入 tkMessageBox 模块，用于提供消息框功能。
# 可以显示信息、警告或错误消息给用户，提供程序运行状态的反馈。
import csv
# 导入 csv 模块，用于处理 CSV (逗号分隔值) 文件。
# 用于将计算结果以结构化的表格形式写入 CSV 文件，便于数据导出和分析。
from multiprocessing import freeze_support
# 导入 multiprocessing 模块的 freeze_support 函数。
# 在 Windows 系统上，当使用 PyInstaller 等工具打包多进程应用时，需要调用此函数以确保程序正常运行。
# 尽管本程序可能没有直接使用多进程，但作为通用实践，包含此行可提高打包后的兼容性。


# ──────────────────────────────── 通用函数 ────────────────────────────────
# 这是一个注释分隔符，用于标记通用函数部分的开始。
# 这部分包含了多个辅助函数，用于执行特定的计算或数据处理任务，是核心计算逻辑的组成单元。

def read_data(file_path):
    # 定义一个名为 read_data 的函数，用于从文件中读取数据。
    """读取 txt 中的数值并剔除含 NaN 行"""
    # 函数的文档字符串，说明其功能是读取 txt 文件中的数值。
    # 并且会剔除包含 NaN (Not a Number) 值的行，确保数据质量。
    data = np.genfromtxt(file_path)
    # 使用 NumPy 的 genfromtxt 函数从指定路径的文件中读取数据。
    # 默认情况下，genfromtxt 会尝试将数据转换为浮点数，并将无法转换的标记为 NaN。
    return data[~np.isnan(data).any(axis=1)]
    # 返回处理后的数据。
    # np.isnan(data) 会创建一个布尔数组，标记 data 中哪些元素是 NaN。
    # .any(axis=1) 检查每一行是否有任何 NaN 值。
    # ~ 操作符对布尔数组取反，即选择不包含 NaN 的行。
    # 最终，此行代码筛选出所有不含 NaN 值的完整数据行，保证后续计算的有效性。


def quadratic_poly_fit(x, y):
    # 定义一个名为 quadratic_poly_fit 的函数，用于执行二次多项式拟合。
    """二次多项式拟合"""
    # 函数的文档字符串，说明其功能是进行二次多项式拟合。
    # 这是为了将离散的应力或温度数据拟合为连续的数学表达式。
    return np.polyfit(x, y, 2)
    # 使用 NumPy 的 polyfit 函数对给定的 x 和 y 数据进行多项式拟合。
    # 第三个参数 2 表示进行二次 (2阶) 多项式拟合。
    # 函数返回拟合多项式的系数，从高次项到低次项排列，例如 [a, b, c] 代表 ax^2 + bx + c。


def integrate_polynomial(coeffs):
    # 定义一个名为 integrate_polynomial 的函数，用于对二次多项式进行积分。
    """二次多项式积分，得到三次多项式系数"""
    # 函数的文档字符串，说明其功能是对二次多项式进行积分。
    # 积分结果将是一个三次多项式，函数返回其系数。
    # 假设输入的 coeffs 是 [a, b, c]，表示 ax^2 + bx + c。
    # 积分结果是 (a/3)x^3 + (b/2)x^2 + cx + D (常数项)。
    return [coeffs[0] / 3.0, coeffs[1] / 2.0, coeffs[2], 0.0]
    # 返回积分后三次多项式的系数。
    # coeffs[0] 是二次项系数，积分后除以 3。
    # coeffs[1] 是一次项系数，积分后除以 2。
    # coeffs[2] 是常数项，积分后变为一次项系数。
    # 0.0 是积分常数 D，在这里设为 0，因为在计算薄膜应力时积分常数通常不影响结果。


def calculate_sigma_p(integ_coeffs, t):
    # 定义一个名为 calculate_sigma_p 的函数，用于计算薄膜应力 σ_p。
    """薄膜应力 σ_p"""
    # 函数的文档字符串，说明其功能是计算薄膜应力 σ_p。
    # integ_coeffs 是积分后的多项式系数，代表应力分布的积分形式。
    # t 是壁厚，是计算薄膜应力的关键几何参数。
    return np.polyval(integ_coeffs, t) / t
    # 使用 NumPy 的 polyval 函数计算在 t 处的积分多项式的值。
    # 然后将结果除以 t，得到薄膜应力 σ_p。
    # 这个公式通常用于计算薄膜应力，其中积分多项式代表应力分布的积分。


def calculate_KI(sigma_p, sigma_q, t, eta):
    # 定义一个名为 calculate_KI 的函数，用于计算应力强度因子 KI。
    """应力强度因子 KI = η·(σ_p M_p + σ_q M_q)·√(πa/10³)/Q
       其中  M_p=1.08、M_q=0.68、Q≈0.9 → 1/ Q≈1.11  已含在旧公式里 """
    # 函数的文档字符串，详细说明了 KI 的计算公式。
    # sigma_p 是薄膜应力，sigma_q 是弯曲应力。
    # t 是壁厚，eta 是 η 系数，一个修正因子，用于调整 KI 的计算。
    # 公式中包含常数 M_p (1.08) 和 M_q (0.68)，以及形状因子 Q (近似为 0.9，其倒数近似为 1.11)。
    # 注意：公式中的 a (裂纹深度) 似乎被替换为 0.25 * t，即裂纹深度为壁厚的 1/4。
    return (eta * (sigma_p * 1.08 + sigma_q * 0.68) * (math.pi * 0.25 * t / 1000.0) ** 0.5 )/ 1.11
    # 根据文档字符串中给出的公式计算 KI。
    # math.pi 是圆周率 π。
    # 0.25 * t 表示裂纹深度 a。
    # / 1000.0 是将 t 从毫米转换为米（如果 t 是毫米单位），确保单位一致性。
    # ** 0.5 是开平方根操作，对应公式中的根号部分。
    # 最终结果除以 1.11，这相当于乘以 1/Q，将形状因子考虑在内。


def calculate_temperature(coeffs, t):
    # 定义一个名为 calculate_temperature 的函数，用于计算特定位置的温度。
    """0.25t 处温度"""
    # 函数的文档字符串，说明其功能是计算在 0.25t 位置处的温度。
    # coeffs 是温度分布的二次多项式系数，通过拟合得到。
    # t 是壁厚，用于确定计算温度的具体位置。
    return np.polyval(coeffs, 0.25 * t)
    # 使用 NumPy 的 polyval 函数，根据温度多项式系数 coeffs。
    # 计算在距离壁厚 0.25 倍位置 (即 0.25 * t) 处的温度值。
    # 这个位置通常被认为是裂纹尖端可能存在的典型位置，其温度对断裂韧度有重要影响。


def calculate_KIc(T, Tk):
    # 定义一个名为 calculate_KIc 的函数，用于计算断裂韧度 KIc。
    """断裂韧度 KIc（公式法，最大 220 MPa·m^0.5）"""
    # 函数的文档字符串，说明其功能是使用公式法计算 KIc。
    # 并指明 KIc 的最大值为 220 MPa·m^0.5，这是一个上限值。
    # T 是当前温度，Tk 是参考温度，两者共同决定 KIc 的值。
    return min(26 + 36 * math.exp(0.02 * (T - Tk)), 220)
    # 根据给定的经验公式计算 KIc。
    # math.exp() 是自然指数函数 e^x，表示 KIc 随温度呈指数增长。
    # 计算结果与 220 进行比较，取两者中的最小值。
    # 确保 KIc 不超过 220 MPa·m^0.5 的上限，符合材料的实际韧性限制。


def assess_KI_vs_KIc(KI, KIc, k):
    # 定义一个名为 assess_KI_vs_KIc 的函数，用于评估脆断结果。
    """评定结论"""
    # 函数的文档字符串，说明其功能是给出评定结论。
    # KI 是计算得到的应力强度因子，代表裂纹扩展的驱动力。
    # KIc 是材料的断裂韧度，代表材料抵抗裂纹扩展的能力。
    # k 是安全系数，用于提供额外的安全裕度。
    return u"通过" if KI < KIc / k else u"不通过"
    # 根据脆断评定标准进行判断。
    # 如果应力强度因子 KI 小于断裂韧度 KIc 除以安全系数 k 的值，则评定为“通过”。
    # 这意味着在考虑安全裕度的情况下，结构是安全的。
    # 否则，评定为“不通过”，表示存在脆断风险。


# ──────────────────────────────── 计算主流程 ────────────────────────────────
# 这是一个注释分隔符，用于标记计算主流程部分的开始。
# 这部分包含了整个脆断评定工具的核心逻辑，协调各个辅助函数的调用。

def main(file_stress, file_temp, t, k,
         eta=1.0, Tk=None, KIc_method=0, fixed_KIc=None):
    # 定义主函数 main，负责协调整个计算过程。
    # file_stress: 应力数据文件的路径，通常是包含时间序列应力分布的文本文件。
    # file_temp: 温度数据文件的路径，通常是包含时间序列温度分布的文本文件。
    # t: 壁厚，关键的几何参数。
    # k: 安全系数，用于评估时的安全裕度。
    # eta: η 系数，默认为 1.0，一个可调节的修正因子。
    # Tk: 参考温度，默认为 None，用于 KIc 的公式计算。
    # KIc_method: KIc 计算方法，0 表示公式法，1 表示固定值，默认为 0。
    # fixed_KIc: 固定 KIc 值，默认为 None，当 KIc_method 为 1 时使用。

    # 1. 读取数据
    # 步骤 1：读取输入数据文件，为后续计算准备数据。
    data_stress = read_data(file_stress)
    # 调用 read_data 函数读取应力数据文件，并剔除无效行。
    data_temp   = read_data(file_temp)
    # 调用 read_data 函数读取温度数据文件，并剔除无效行。

    num_pts = data_stress.shape[1] - 1
    # 计算每个时间步的数据点数量。
    # data_stress.shape[1] 是列数，第一列是时间，所以减去 1 得到实际数据点数（即空间离散点数）。
    x = np.linspace(0, t, num_pts)
    # 使用 NumPy 的 linspace 函数生成一个等间距的数组 x。
    # 这个数组代表了从 0 到壁厚 t 的位置，用于多项式拟合的自变量（即空间坐标）。

    # 2. 初始化结果容器
    # 步骤 2：初始化列表，用于存储每个时间步的计算结果。
    time_col = data_stress[:, 0]
    # 提取应力数据的第一列，即时间列，作为结果输出的时间戳。
    sigma_p_list, sigma_q_list = [], []
    # 初始化薄膜应力 (sigma_p) 和弯曲应力 (sigma_q) 的空列表，用于存储每个时间步的计算结果。
    KI_list, temp_list, KIc_list, result_list = [], [], [], []
    # 初始化应力强度因子 (KI)、温度 (temp)、断裂韧度 (KIc) 和评估结果 (result) 的空列表，用于存储每个时间步的计算结果。

    # 3. 循环处理每一行
    # 步骤 3：遍历每一行数据（即每个时间步），进行详细计算。
    for i in range(data_stress.shape[0]):
        # 遍历应力数据的每一行。data_stress.shape[0] 是行数，每一行代表一个时间步。
        y_stress = data_stress[i, 1:]
        # 提取当前时间步的应力数据（从第二列开始，即剔除时间列），这些是空间上的应力分布。
        y_temp   = data_temp[i,   1:]
        # 提取当前时间步的温度数据（从第二列开始，即剔除时间列），这些是空间上的温度分布。

        # ─ 应力 ─
        # 应力计算部分，包括薄膜应力、弯曲应力和应力强度因子。
        coeffs_stress = quadratic_poly_fit(x, y_stress)
        # 对当前时间步的应力数据进行二次多项式拟合，得到应力分布的系数。
        coeffs_int    = integrate_polynomial(coeffs_stress)
        # 对拟合得到的应力多项式进行积分，得到积分多项式的系数，这是计算薄膜应力的中间步骤。
        sigma_p = calculate_sigma_p(coeffs_int, t)
        # 根据积分多项式系数和壁厚 t，计算薄膜应力 sigma_p。
        sigma_q = data_stress[i, 1] - sigma_p
        # 计算弯曲应力 sigma_q。这里假设 data_stress[i, 1] 是总应力或表面应力，弯曲应力是总应力减去薄膜应力。

        KI = calculate_KI(sigma_p, sigma_q, t, eta)
        # 调用 calculate_KI 函数，计算当前时间步的应力强度因子 KI，这是脆断评定的核心参数之一。

        # ─ 温度 ─
        # 温度计算部分，获取裂纹尖端附近的温度。
        coeffs_temp = quadratic_poly_fit(x, y_temp)
        # 对当前时间步的温度数据进行二次多项式拟合，得到温度分布的系数。
        T = calculate_temperature(coeffs_temp, t)
        # 调用 calculate_temperature 函数，计算在 0.25t 位置处的温度 T，该温度用于确定材料的断裂韧度。

        # ─ KIc ─
        # 断裂韧度 KIc 计算部分，根据用户选择的方法确定 KIc。
        if KIc_method == 0:
            # 如果 KIc 计算方法是 0 (公式法)，则根据温度动态计算 KIc。
            KIc = calculate_KIc(T, Tk)
            # 调用 calculate_KIc 函数，根据当前温度 T 和参考温度 Tk 计算 KIc。
        else:
            # 如果 KIc 计算方法是 1 (固定值法)，则直接使用用户预设的 KIc 值。
            KIc = fixed_KIc
            # 直接使用用户输入的固定 KIc 值，不依赖于温度变化。

        verdict = assess_KI_vs_KIc(KI, KIc, k)
        # 调用 assess_KI_vs_KIc 函数，根据计算得到的 KI、KIc 和安全系数 k 给出脆断评定结论。

        # 收集
        # 将当前时间步的计算结果添加到各自的列表中，以便后续统一输出。
        sigma_p_list.append(sigma_p)
        # 将计算得到的薄膜应力添加到列表中。
        sigma_q_list.append(sigma_q)
        # 将计算得到的弯曲应力添加到列表中。
        KI_list.append(KI)
        # 将计算得到的应力强度因子添加到列表中。
        temp_list.append(T)
        # 将计算得到的温度添加到列表中。
        KIc_list.append(KIc)
        # 将计算得到的断裂韧度添加到列表中。
        result_list.append(verdict)
        # 将评估结果（“通过”或“不通过”）添加到列表中。

    # 4. 保存 CSV（GBK 编码）
    # 步骤 4：将所有计算结果保存到 CSV 文件，方便用户查看和进一步分析。
    out_csv = u"脆断评定结果.csv"
    # 定义输出 CSV 文件的文件名。使用 Unicode 字符串以支持中文文件名。
    with open(out_csv, "wb") as f:
        # 以二进制写入模式 ("wb") 打开 CSV 文件。
        # "wb" 模式在 Python 2 中用于写入非 ASCII 字符时避免编码问题，并允许 csv 模块处理行结束符。
        writer = csv.writer(f)
        # 创建一个 csv.writer 对象，用于向文件中写入数据。

        headers = [u"时间", u"薄膜应力", u"弯曲应力",
                   u"应力强度因子", u"温度", u"断裂韧度",
                   u"评估结果"]
        # 定义 CSV 文件的表头。使用 Unicode 字符串以支持中文表头。
        writer.writerow([h.encode("gbk") for h in headers])
        # 将表头写入 CSV 文件。
        # 遍历 headers 列表，将每个 Unicode 字符串编码为 GBK 格式，以确保中文在 Excel 等软件中正确显示。

        for i in range(len(time_col)):
            # 遍历所有时间步的计算结果，逐行写入 CSV 文件。
            writer.writerow([
                time_col[i],
                # 写入当前时间步的时间。
                sigma_p_list[i],
                # 写入当前时间步的薄膜应力。
                sigma_q_list[i],
                # 写入当前时间步的弯曲应力。
                KI_list[i],
                # 写入当前时间步的应力强度因子。
                temp_list[i],
                # 写入当前时间步的温度。
                KIc_list[i],
                # 写入当前时间步的断裂韧度。
                result_list[i].encode("gbk")
                # 写入当前时间步的评估结果，并编码为 GBK，确保中文显示。
            ])

    messagebox.showinfo(u"完成", u"计算结果已保存到 {}".format(out_csv))
    # 弹出信息框，告知用户计算已完成，并显示结果文件的保存路径，提供友好的用户反馈。


# ──────────────────────────────── 图形界面 ────────────────────────────────
# 这是一个注释分隔符，用于标记图形界面部分的开始。
# 这部分包含了使用 Tkinter 构建用户界面的代码，实现了用户交互功能。

def run_gui():
    # 定义 run_gui 函数，用于创建和运行图形用户界面。

    # ─ 内部工具函数 ─
    # 内部辅助函数部分，这些函数仅在 run_gui 作用域内使用。
    def browse(file_tag):
        # 定义 browse 函数，用于处理文件浏览按钮的点击事件。
        # file_tag 用于区分是应力文件还是温度文件，以便更新正确的输入框。
        path = filedialog.askopenfilename(
            # 弹出文件选择对话框，让用户选择文件。
            filetypes=[(u"{} 文件".format(file_tag), "*.txt")])
            # 设置文件类型过滤器，只显示 .txt 文件，并根据 file_tag 显示不同的描述，提高用户体验。
        if not path:
            # 如果用户没有选择文件（点击了取消），则直接返回，不进行任何操作。
            return
        if file_tag == u"应力":
            # 如果是应力文件浏览按钮触发的。
            ent_stress.delete(0, tk.END)
            # 清空应力文件路径输入框的当前内容。
            ent_stress.insert(0, path)
            # 将选择的文件路径插入到应力文件路径输入框中。
        else:
            # 如果是温度文件浏览按钮触发的。
            ent_temp.delete(0, tk.END)
            # 清空温度文件路径输入框的当前内容。
            ent_temp.insert(0, path)
            # 将选择的文件路径插入到温度文件路径输入框中。

    def start():
        # 定义 start 函数，用于处理“开始计算”按钮的点击事件。
        # 这是 GUI 与核心计算逻辑的连接点。
        try:
            # 使用 try-except 块捕获可能发生的错误，例如用户输入无效数据。
            f_stress = ent_stress.get()
            # 获取应力文件路径输入框的内容。
            f_temp   = ent_temp.get()
            # 获取温度文件路径输入框的内容。
            t_val  = float(ent_t.get())
            # 获取壁厚输入框的内容，并尝试转换为浮点数。
            k_val  = float(ent_k.get())
            # 获取安全系数输入框的内容，并尝试转换为浮点数。
            eta_val = float(ent_eta.get())    # ← 取 η
            # 获取 η 系数输入框的内容，并尝试转换为浮点数。

            method  = int(var_method.get())
            # 获取 KIc 计算方式单选按钮的选中值 (0 或 1)，并转换为整数。
            Tk_val  = float(ent_Tk.get())  if method == 0 else None
            # 如果计算方式是公式法 (0)，则获取参考温度 Tk 的值并转换为浮点数，否则为 None。
            KIc_val = float(ent_KIc.get()) if method == 1 else None
            # 如果计算方式是固定值法 (1)，则获取固定 KIc 的值并转换为浮点数，否则为 None。

            main(f_stress, f_temp, t_val, k_val,
                 eta=eta_val,
                 Tk=Tk_val, KIc_method=method, fixed_KIc=KIc_val)
            # 调用主计算函数 main，传入从 GUI 获取的所有参数。
        except Exception as e:
            # 捕获所有类型的异常，例如文件不存在、输入非数字等。
            messagebox.showerror(u"错误", u"发生错误：{}".format(unicode(e)))
            # 弹出错误信息框，显示发生的错误内容。unicode(e) 确保错误信息能正确显示中文，提高错误提示的友好性。

    # ─ 创建窗口 ─
    # GUI 窗口创建部分，定义了主窗口的属性和布局。
    root = tk.Tk()
    # 创建 Tkinter 应用程序的主窗口实例。
    root.title(u"防脆断评定工具")
    # 设置窗口的标题，显示工具的名称。
    root.geometry("880x520")
    # 设置窗口的初始大小为 880x520 像素，提供一个合适的默认尺寸。
    root.configure(bg="#f0f0f0")
    # 设置窗口的背景颜色为浅灰色 (#f0f0f0)，使界面看起来更柔和。

    f_large = ("Microsoft YaHei", 12)
    # 定义一个大字体样式，使用“微软雅黑”字体，字号 12，用于重要的标签。
    f_mid   = ("Microsoft YaHei", 10)
    # 定义一个中等字体样式，使用“微软雅黑”字体，字号 10，用于输入框和按钮。

    # 行 0 : 应力文件
    # GUI 布局：第一行，用于应力数据文件选择。
    tk.Label(root, text=u"应力数据文件:", bg="#f0f0f0", font=f_large
             ).grid(row=0, column=0, padx=10, pady=10, sticky="w")
    # 创建一个标签，显示“应力数据文件:”，并将其放置在网格布局的第 0 行第 0 列。
    # bg 设置背景色，font 设置字体，padx/pady 设置内边距，sticky="w" 使其左对齐。
    ent_stress = tk.Entry(root, width=70, font=f_mid)
    # 创建一个输入框，用于显示或输入应力文件路径，宽度设置为 70 个字符。
    ent_stress.grid(row=0, column=1, padx=10, pady=10)
    # 将输入框放置在第 0 行第 1 列，并设置内边距。
    tk.Button(root, text=u"浏览", command=lambda: browse(u"应力"),
              bg="teal", fg="white", font=f_mid
              ).grid(row=0, column=2, padx=10, pady=10)
    # 创建一个“浏览”按钮，点击时调用 browse 函数并传入“应力”作为参数。
    # 设置按钮的背景色 (teal)、前景色 (white) 和字体，并将其放置在第 0 行第 2 列。

    # 行 1 : 温度文件
    # GUI 布局：第二行，用于温度数据文件选择。
    tk.Label(root, text=u"温度数据文件:", bg="#f0f0f0", font=f_large
             ).grid(row=1, column=0, padx=10, pady=10, sticky="w")
    # 创建温度数据文件标签，放置在第 1 行第 0 列。
    ent_temp = tk.Entry(root, width=70, font=f_mid)
    # 创建温度文件路径输入框，宽度设置为 70 个字符。
    ent_temp.grid(row=1, column=1, padx=10, pady=10)
    # 放置温度文件路径输入框在第 1 行第 1 列。
    tk.Button(root, text=u"浏览", command=lambda: browse(u"温度"),
              bg="teal", fg="white", font=f_mid
              ).grid(row=1, column=2, padx=10, pady=10)
    # 创建温度文件“浏览”按钮，点击时调用 browse 函数并传入“温度”作为参数。

    # 行 2 : 壁厚 t
    # GUI 布局：第三行，用于输入壁厚 t。
    tk.Label(root, text=u"壁厚 (t):", bg="#f0f0f0", font=f_large
             ).grid(row=2, column=0, padx=10, pady=10, sticky="w")
    # 创建壁厚标签，放置在第 2 行第 0 列。
    ent_t = tk.Entry(root, font=f_mid)
    # 创建壁厚输入框。
    ent_t.grid(row=2, column=1, padx=10, pady=10, sticky="w")
    # 放置壁厚输入框在第 2 行第 1 列，并左对齐。

    # 行 3 : 安全系数 k
    # GUI 布局：第四行，用于输入安全系数 k。
    tk.Label(root, text=u"安全系数 (k):", bg="#f0f0f0", font=f_large
             ).grid(row=3, column=0, padx=10, pady=10, sticky="w")
    # 创建安全系数标签，放置在第 3 行第 0 列。
    ent_k = tk.Entry(root, font=f_mid)
    # 创建安全系数输入框。
    ent_k.grid(row=3, column=1, padx=10, pady=10, sticky="w")
    # 放置安全系数输入框在第 3 行第 1 列。

    # 行 4 : η 系数
    # GUI 布局：第五行，用于输入 η 系数。
    tk.Label(root, text=u"η 系数:", bg="#f0f0f0", font=f_large
             ).grid(row=4, column=0, padx=10, pady=10, sticky="w")
    # 创建 η 系数标签，放置在第 4 行第 0 列。
    ent_eta = tk.Entry(root, font=f_mid)
    # 创建 η 系数输入框。
    ent_eta.insert(0, "1.0")           # 默认 1.0
    # 为 η 系数输入框设置默认值 1.0，方便用户使用。
    ent_eta.grid(row=4, column=1, padx=10, pady=10, sticky="w")
    # 放置 η 系数输入框在第 4 行第 1 列。

    # 行 5 : 参考温度 Tk
    # GUI 布局：第六行，用于输入参考温度 Tk。
    tk.Label(root, text=u"参考温度 (Tk):", bg="#f0f0f0", font=f_large
             ).grid(row=5, column=0, padx=10, pady=10, sticky="w")
    # 创建参考温度 Tk 标签，放置在第 5 行第 0 列。
    ent_Tk = tk.Entry(root, font=f_mid)
    # 创建参考温度 Tk 输入框。
    ent_Tk.grid(row=5, column=1, padx=10, pady=10, sticky="w")
    # 放置参考温度 Tk 输入框在第 5 行第 1 列。

    # 行 6 : KIc 计算方式
    # GUI 布局：第七行，用于选择 KIc 计算方式（公式法或固定值法）。
    tk.Label(root, text=u"KIc 计算方式:", bg="#f0f0f0", font=f_large
             ).grid(row=6, column=0, padx=10, pady=10, sticky="w")
    # 创建 KIc 计算方式标签，放置在第 6 行第 0 列。

    frame_method = tk.Frame(root, bg="#f0f0f0")
    # 创建一个 Frame 控件，用于容纳 KIc 计算方式的单选按钮，以便更好地组织布局。
    frame_method.grid(row=6, column=1, columnspan=2,
                      padx=10, pady=10, sticky="w")
    # 将 Frame 放置在第 6 行，跨越 2 列，并左对齐。

    var_method = tk.StringVar(value="0")
    # 创建一个 Tkinter 字符串变量，用于存储单选按钮的选中值。
    # 默认值为 "0"，表示“按公式计算”被选中。
    tk.Radiobutton(frame_method, text=u"按公式计算",
                   variable=var_method, value="0",
                   font=f_mid, bg="#f0f0f0"
                   ).pack(side="left", padx=5)
    # 创建第一个单选按钮：“按公式计算”。
    # variable 绑定到 var_method，value 设为 "0"。pack(side="left") 使其在 Frame 中左对齐排列。
    tk.Radiobutton(frame_method, text=u"使用固定值",
                   variable=var_method, value="1",
                   font=f_mid, bg="#f0f0f0"
                   ).pack(side="left", padx=5)
    # 创建第二个单选按钮：“使用固定值”。
    # variable 绑定到 var_method，value 设为 "1"。

    # 行 7 : 固定 KIc 值
    # GUI 布局：第八行，用于输入固定 KIc 值。
    tk.Label(root, text=u"固定 KIc 值:", bg="#f0f0f0", font=f_large
             ).grid(row=7, column=0, padx=10, pady=10, sticky="w")
    # 创建固定 KIc 值标签，放置在第 7 行第 0 列。
    ent_KIc = tk.Entry(root, font=f_mid)
    # 创建固定 KIc 值输入框。
    ent_KIc.grid(row=7, column=1, padx=10, pady=10, sticky="w")
    # 放置固定 KIc 值输入框在第 7 行第 1 列。

    # 行 8 : 开始按钮
    # GUI 布局：第九行，用于放置“开始计算”按钮。
    tk.Button(root, text=u"开始计算", command=start,
              bg="teal", fg="white",
              font=("Microsoft YaHei", 14), width=20
              ).grid(row=8, column=0, columnspan=3, pady=25)
    # 创建“开始计算”按钮。
    # command 绑定到 start 函数，点击时执行计算逻辑。
    # 设置按钮的背景色 (teal)、前景色 (white)、字体和宽度 (20)。
    # columnspan=3 使按钮跨越 3 列，pady 设置垂直内边距，使其居中显示。

    root.mainloop()
    # 启动 Tkinter 事件循环。
    # 这会使窗口保持打开状态，并响应用户的交互（如点击按钮、输入文本），直到窗口关闭。


# ──────────────────────────────── 独立启动 ────────────────────────────────
# 这是一个注释分隔符，用于标记程序独立启动部分的开始。

if __name__ == "__main__":
    # 这是一个标准的 Python 惯用法，确保以下代码只在脚本作为主程序运行时执行。
    # 而不是在被其他模块导入时执行，避免不必要的副作用。
    freeze_support()   # Windows 下多进程安全
    # 在 Windows 系统上，当使用 PyInstaller 等工具打包程序时，
    # 如果程序中使用了 multiprocessing 模块，需要调用 freeze_support() 来确保多进程的正确启动。
    run_gui()
    # 调用 run_gui 函数，启动图形用户界面，使程序开始运行并等待用户交互。