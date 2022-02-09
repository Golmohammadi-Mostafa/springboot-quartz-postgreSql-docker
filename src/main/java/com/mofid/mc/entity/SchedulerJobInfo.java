package com.mofid.mc.entity;

import lombok.*;

import javax.persistence.*;

@EqualsAndHashCode
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@Data
@Entity
@Table(name = "qrtz_scheduler_job_info")
public class SchedulerJobInfo {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "scheduler_info_seq")
    @SequenceGenerator(name = "scheduler_info_seq", sequenceName = "scheduler_info_id_seq", allocationSize = 1)
    @Column(name = "id")
    private Long id;

    @Column(name = "job_name", unique = true, nullable = false)
    private String jobName;

    @Column(name = "job_group", nullable = false)
    private String jobGroup;

    @Column(name = "job_status")
    private String jobStatus;

    @Column(name = "job_class", unique = true, nullable = false)
    private String jobClass;

    @Column(name = "cron_expression")
    private String cronExpression;

    @Column(name = "repeat_time")
    private Long repeatTime;

    @Column(name = "cron_job")
    private Boolean cronJob;

    @Column(name = "enabled")
    private Boolean enabled;
}
